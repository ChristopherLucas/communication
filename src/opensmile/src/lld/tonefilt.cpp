/*F***************************************************************************
 * 
 * openSMILE - the Munich open source Multimedia Interpretation by 
 * Large-scale Extraction toolkit
 * 
 * This file is part of openSMILE.
 * 
 * openSMILE is copyright (c) by audEERING GmbH. All rights reserved.
 * 
 * See file "COPYING" for details on usage rights and licensing terms.
 * By using, copying, editing, compiling, modifying, reading, etc. this
 * file, you agree to the licensing terms in the file COPYING.
 * If you do not agree to the licensing terms,
 * you must immediately destroy all copies of this file.
 * 
 * THIS SOFTWARE COMES "AS IS", WITH NO WARRANTIES. THIS MEANS NO EXPRESS,
 * IMPLIED OR STATUTORY WARRANTY, INCLUDING WITHOUT LIMITATION, WARRANTIES OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, ANY WARRANTY AGAINST
 * INTERFERENCE WITH YOUR ENJOYMENT OF THE SOFTWARE OR ANY WARRANTY OF TITLE
 * OR NON-INFRINGEMENT. THERE IS NO WARRANTY THAT THIS SOFTWARE WILL FULFILL
 * ANY OF YOUR PARTICULAR PURPOSES OR NEEDS. ALSO, YOU MUST PASS THIS
 * DISCLAIMER ON WHENEVER YOU DISTRIBUTE THE SOFTWARE OR DERIVATIVE WORKS.
 * NEITHER TUM NOR ANY CONTRIBUTOR TO THE SOFTWARE WILL BE LIABLE FOR ANY
 * DAMAGES RELATED TO THE SOFTWARE OR THIS LICENSE AGREEMENT, INCLUDING
 * DIRECT, INDIRECT, SPECIAL, CONSEQUENTIAL OR INCIDENTAL DAMAGES, TO THE
 * MAXIMUM EXTENT THE LAW PERMITS, NO MATTER WHAT LEGAL THEORY IT IS BASED ON.
 * ALSO, YOU MUST PASS THIS LIMITATION OF LIABILITY ON WHENEVER YOU DISTRIBUTE
 * THE SOFTWARE OR DERIVATIVE WORKS.
 * 
 * Main authors: Florian Eyben, Felix Weninger, 
 * 	      Martin Woellmer, Bjoern Schuller
 * 
 * Copyright (c) 2008-2013, 
 *   Institute for Human-Machine Communication,
 *   Technische Universitaet Muenchen, Germany
 * 
 * Copyright (c) 2013-2015, 
 *   audEERING UG (haftungsbeschraenkt),
 *   Gilching, Germany
 * 
 * Copyright (c) 2016,	 
 *   audEERING GmbH,
 *   Gilching Germany
 ***************************************************************************E*/


/*  openSMILE component:

on-line semi-tone filter bank

*/

// TODO: this should be a WinToVecProcessor and NOT a DataProcessor !


#include <lld/tonefilt.hpp>
//#include <math.h>

#define MODULE "cTonefilt"


SMILECOMPONENT_STATICS(cTonefilt)

SMILECOMPONENT_REGCOMP(cTonefilt)
{
  SMILECOMPONENT_REGCOMP_INIT
  scname = COMPONENT_NAME_CTONEFILT;
  sdescription = COMPONENT_DESCRIPTION_CTONEFILT;

  // we inherit cDataProcessor configType and extend it:
  SMILECOMPONENT_INHERIT_CONFIGTYPE("cDataProcessor")
  SMILECOMPONENT_IFNOTREGAGAIN(
    ct->setField("nameAppend", nullptr, "tonefilt");
    ct->setField("nNotes","The number of semi-tone filters in the bank",48);
    ct->setField("firstNote","The frequency of the first note in Hz",55.0);
    ct->setField("decayF0","The gliding average decay coefficient for the first note (lowest frequency)",0.9995);
    ct->setField("decayFN","The gliding average decay coefficient for the last note (highest frequency) (must be < decayF0!); decay coefficents for intermediate frequencies will be interpolated linearly from the start and end coefficients.",0.998);
    ct->setField("outputPeriod","Specifies the period at which to produce output frames in seconds",0.1);
    //ct->setField("outputBuffersize","size of output buffer in frames (0=determine automatically)",0);
  )

  SMILECOMPONENT_MAKEINFO(cTonefilt);
}

SMILECOMPONENT_CREATE(cTonefilt)

//-----

cTonefilt::cTonefilt(const char *_name) :
  cDataProcessor(_name),
  outputPeriod(0.0),
  //outputBuffersize(0),
  tmpFrame(nullptr),
  tmpVec(nullptr),
  freq(nullptr),
  pos(nullptr),
  corrC(nullptr), corrS(nullptr),
  decayF(nullptr)
{

}

void cTonefilt::fetchConfig()
{
  cDataProcessor::fetchConfig();
  
  outputPeriod = getDouble("outputPeriod");
  if (outputPeriod <= 0.0) outputPeriod = 0.1;
  SMILE_DBG(2,"outputPeriod = %f s",outputPeriod);

  decayFN = getDouble("decayFN");
  if (decayFN < 0.0) decayFN = 0.0;
  if (decayFN > 1.0) decayFN = 1.0;
  SMILE_DBG(2,"decayFN = %f",decayFN);

  decayF0 = getDouble("decayF0");
  if (decayF0 < decayFN) decayF0 = decayFN;
  if (decayF0 < 0.0) decayF0 = 0.0;
  if (decayF0 > 1.0) decayF0 = 1.0;
  SMILE_DBG(2,"decayF0 = %f",decayF0);

  firstNote = getDouble("firstNote");
  if (firstNote <= 0.0) firstNote = 1.0;
  SMILE_DBG(2,"firstNote = %f Hz",firstNote);

  nNotes = getInt("nNotes");
  if (nNotes < 1) nNotes = 1;
  SMILE_DBG(2,"nNotes = %i",nNotes);

  /*
  if (isSet("outputBuffersize")) {
    outputBuffersize = getInt("outputBuffersize");
    SMILE_DBG(2,"outputBuffersize = %i frames",outputBuffersize);
  }
  */
}


int cTonefilt::configureWriter(sDmLevelConfig &c)
{
  SMILE_DBG(3,"reader period = %f",c.T);

  // c.T is input period here...
  if (c.T != 0.0) {
    outputPeriodFrames = (long)round(outputPeriod / c.T);
  } else {
    outputPeriodFrames = (long)round(outputPeriod);
  }

/*
  if (!fsfGiven) frameSizeFrames = (long)round(frameSize / c->T);
  else frameSize = ((double)frameSizeFrames) * c->T;
  if (!fstfGiven) frameStepFrames = (long)round(frameStep / c->T);
  else frameStep = ((double)frameStepFrames) * c->T;
  SMILE_DBG(4,"computed frameSizeFrames = %i",frameSizeFrames);
  SMILE_DBG(4,"computed frameStepFrames = %i",frameStepFrames);
*/

  if (outputPeriod < c.T) {
    outputPeriod = c.T;
    outputPeriodFrames = 1;
  }
  if (outputPeriodFrames < 0) outputPeriodFrames = 0;

  c.T = outputPeriod;
  c.frameSizeSec = outputPeriod;
  
  reader_->setupSequentialMatrixReading(outputPeriodFrames, outputPeriodFrames);

  // you must return 1, in order to indicate configure success (0 indicated failure)
  return 1;
}


int cTonefilt::setupNewNames(long nEl)
{
  // get reader names, append tonefilt to them, and set writer names
  N = reader_->getLevelN();
  Nf = reader_->getLevelNf();
  inputPeriod = reader_->getLevelT();
  int i,n;
  char *xx;

  corrC = (double**)calloc(1,sizeof(double*)*N);
  corrS = (double**)calloc(1,sizeof(double*)*N);

  for (i=0; i<Nf; i++) {
    int __N=0;
    const char *tmp = reader_->getFieldName(i,&__N);
    if (tmp == nullptr) { SMILE_ERR(1,"reader->getFieldName(%i) failed (return value = nullptr)!",i); return 0; }
    if (__N > 1) {
      for (n=0; n<__N; n++) {
        if ((nameAppend_!=nullptr)&&(strlen(nameAppend_)>0))
          xx = myvprint("%s%i_%s",tmp,n,nameAppend_);
        else
          xx = myvprint("%s%i",tmp,n);
//        char *xx = myvprint("%s%i_frame",tmp,n);  // TODO: append name of winFunc, if option in config file is set
        writer_->addField( xx, nNotes );
        free(xx);
      }
    } else {
      if ((nameAppend_!=nullptr)&&(strlen(nameAppend_)>0))
        xx = myvprint("%s_%s",tmp,nameAppend_);
      else
        xx = myvprint("%s",tmp);
//      char *xx = myvprint("%s_frame",tmp);  // TODO: append name of winFunc, if option in config file is set
      writer_->addField( xx, nNotes );
      free(xx);
    }
  }
  
  for (i=0; i<N; i++) {
    // initialize buffers for each element we process...
    corrC[i] = (double*)calloc(1,sizeof(double)*nNotes);
    corrS[i] = (double*)calloc(1,sizeof(double)*nNotes);
  }
  // initialize global buffers:
  freq = (double*)malloc(sizeof(double)*nNotes);
  decayF = (double*)malloc(sizeof(double)*nNotes);  // decay coefficient for each note
  for (n=0; n<nNotes; n++) {
    freq[n] = firstNote * pow(2.0,(double)n / 12.0); // freq. for each note
  }
  for (n=0; n<nNotes; n++) {
    // TODO!!!!!!!!!!
    //decayF[n] = decayFN + (decayF0-decayFN) * ((double)nNotes/12.0) / pow(2.0, (double)n/12.0); // TODO
    decayF[n] = decayFN + (decayF0-decayFN) * (freq[n]-freq[0])/(freq[nNotes-1]);
  }
  pos = (long *)calloc(1,sizeof(long)*N);
  
  namesAreSet_ = 1;
  if (tmpFrame==nullptr) tmpFrame=(FLOAT_DMEM*)calloc(1,sizeof(FLOAT_DMEM)*nNotes);
  return 1;
}

/*
int cTonefilt::myFinaliseInstance()
{
  return cDataProcessor::myFinaliseInstance();
}
*/

void cTonefilt::doFilter(int i, cMatrix *row, FLOAT_DMEM*y)
{
  long n,t;
  double *s = corrS[i]; // sine
  double *c = corrC[i]; // cosine
  // TOOD: check for type dataF
  for (t=0; t<nNotes; t++) {
    double f= freq[t];
    FLOAT_DMEM *x = row->dataF;
    long idx = pos[i];
    for (n=0; n<row->nT; n++) {
      // compute correlation with sin+cos
      double time = (double)(idx+n) * inputPeriod;
      //      s[t] += sin(2.0*M_PI*f*t) * x[n];
      //      c[t] += cos(2.0*M_PI*f*t) * x[n];
      s[t] = decayF[t] * s[t] + (1.0 - decayF[t]) * sin(2.0*M_PI*f*time) * (double)x[n];
      c[t] = decayF[t] * c[t] + (1.0 - decayF[t]) * cos(2.0*M_PI*f*time) * (double)x[n];
    }
    y[t] = (FLOAT_DMEM)sqrt(c[t]*c[t] + s[t]*s[t]);  // sqrt(re^2 + im^2)
    y[t] *= 10.0;
  }
  pos[i] += row->nT;
}

int cTonefilt::myTick(long long t)
{
  SMILE_DBG(5,"tick # %i, cTonefilt ....",t);

  if (!(writer_->checkWrite(1))) return 0;

  // get next frame from dataMemory
  cMatrix *mat = reader_->getNextMatrix();
  if (mat == nullptr) { return 0; } // currently no data available

  if (tmpVec==nullptr) tmpVec = new cVector(nNotes*N,mat->type);
//  printf("vs=%i Nf=%i nn=%i\n",tmpVec->N,Nf,nNotes);
  
  int i;
  for (i=0; i<N; i++) {
    cMatrix *r = mat->getRow(i);

    doFilter(i,r,tmpFrame); 
    
    // copy data into main vector
    memcpy( tmpVec->dataF+i*nNotes, tmpFrame, sizeof(FLOAT_DMEM)*nNotes );
  }

  // generate new tmeta from first and last tmeta
  mat->tmetaSquash();
  tmpVec->tmetaReplace(mat->tmeta);
  
  // save to dataMemory
  writer_->setNextFrame(tmpVec);

  return 1;
}


cTonefilt::~cTonefilt()
{
  if (tmpFrame!=nullptr) free(tmpFrame);
  if (tmpVec!=nullptr) delete tmpVec;
  if (freq!=nullptr) free(freq);
  if (pos!=nullptr) free(pos);
  if (decayF !=nullptr) free(decayF);
  int i;
  if (corrS !=nullptr) {
    for (i=0; i<N; i++) { if (corrS[i] != nullptr) free(corrS[i]); }
    free(corrS);
  }
  if (corrC !=nullptr) {
    for (i=0; i<N; i++) { if (corrC[i] != nullptr) free(corrC[i]); }
    free(corrC);
  }
}

