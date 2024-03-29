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


/*  openSMILE component: cMfcc

computes Mel-Frequency-Cepstral Coefficients (MFCC) from Mel-Spectrum

*/

#include <R.h>
#include <Rdefines.h>
#include <lldcore/mfcc.hpp>

#define MODULE "cMfcc"


SMILECOMPONENT_STATICS(cMfcc)

SMILECOMPONENT_REGCOMP(cMfcc)
{
  SMILECOMPONENT_REGCOMP_INIT
  scname = COMPONENT_NAME_CMFCC;
  sdescription = COMPONENT_DESCRIPTION_CMFCC;

  // we inherit cVectorProcessor configType and extend it:
  SMILECOMPONENT_INHERIT_CONFIGTYPE("cVectorProcessor")

  SMILECOMPONENT_IFNOTREGAGAIN(
    ct->setField("nameAppend", nullptr, "mfcc");
    ct->setField("firstMfcc","The first MFCC to compute",1);
    ct->setField("lastMfcc","The last MFCC to compute",12);
    ct->setField("nMfcc","Use this option to specify the number of MFCC, instead of specifying lastMfcc",12);
    ct->setField("melfloor","The minimum value allowed for melspectra when taking the log spectrum (this parameter will be forced to 1.0 when htkcompatible=1)",0.00000001); 
    ct->setField("doLog", "This defaults to 1 (on), set it to 0 to disable the log() operation on the (power) spectrum before applying the DCT. Note: If disabled, the output cannot be considered Cepstral coefficients anymore!", 1);
    ct->setField("cepLifter","Parameter for cepstral 'liftering', set this to 0.0 to disable cepstral liftering",22.0);
    ct->setField("htkcompatible","1 = append the 0-th coefficient at the end instead of placing it as the first element of the output vector",1);
    
    ct->setField("inverse","1/0 = on/off : comutation of inverse MFCC (i.e. input is MFCC array)",0);
    ct->setField("nBands","number of mel/bark bands to create when computing the inverse MFCC (must be the same as the number of bands the forward transform was performed on).",26);
    ct->setField("printDctBaseFunctions", "1/0 = on/off: print the dct base functions in octave compatible syntax to standard output", 0);
  )

  SMILECOMPONENT_MAKEINFO(cMfcc);
}

SMILECOMPONENT_CREATE(cMfcc)

//-----

cMfcc::cMfcc(const char *_name) :
  cVectorProcessor(_name),
  htkcompatible(0),  
  costable(nullptr),
  sintable(nullptr),
  firstMfcc(1),
  lastMfcc(12),
  doLog_(1)
{}

void cMfcc::fetchConfig()
{
  cVectorProcessor::fetchConfig();
  
  firstMfcc = getInt("firstMfcc");
  SMILE_IDBG(2,"firstMfcc = %i",firstMfcc);
  lastMfcc = getInt("lastMfcc");
  melfloor = (FLOAT_DMEM)getDouble("melfloor");
  SMILE_IDBG(2,"melfloor = %f", melfloor);
  doLog_ = getInt("doLog");
  SMILE_IDBG(2,"doLog = %f", doLog_);  
  cepLifter = (FLOAT_DMEM)getDouble("cepLifter");
  SMILE_IDBG(2,"cepLifter = %f", cepLifter);

  if (!isSet("lastMfcc")&&isSet("nMfcc")) {
    nMfcc = getInt("nMfcc");
    lastMfcc = firstMfcc + nMfcc - 1;
  } else {
    nMfcc = lastMfcc - firstMfcc + 1;
  }
  SMILE_IDBG(2,"lastMfcc = %i",lastMfcc);
  SMILE_IDBG(2,"nMfcc = %i",nMfcc);

  htkcompatible = getInt("htkcompatible");
  if (htkcompatible) {
    SMILE_IDBG(2,"HTK compatible output is enabled");
	melfloor = 1.0;
  }

  inverse=getInt("inverse");
  nBands=getInt("nBands");
  printDctBaseFunctions=getInt("printDctBaseFunctions");
}

int cMfcc::dataProcessorCustomFinalise()
{
  // allocate for multiple configurations..
  if (sintable == nullptr) sintable = (FLOAT_DMEM**)multiConfAlloc();
  if (costable == nullptr) costable = (FLOAT_DMEM**)multiConfAlloc();

  return cVectorProcessor::dataProcessorCustomFinalise();
}

int cMfcc::setupNamesForField(int i, const char*name, long nEl)
{
  // compute sin/cos tables here:
  if (inverse) {
    initTables(nBands,getFconf(i));
    name = "iMelspec";
    if ((nameAppend_ != nullptr)&&(strlen(nameAppend_)>0)) {
      addNameAppendField(name,nameAppend_,nBands,0);
    } else {
      writer_->addField( name, nBands, 0);
    }
    return nBands;
  } else {
    initTables(nEl,getFconf(i));
    if ((nameAppend_ != nullptr)&&(strlen(nameAppend_)>0)) {
      if (!copyInputName_) {
        name="";
      }
      addNameAppendField(name,nameAppend_,nMfcc,firstMfcc);
    } else {
      writer_->addField( name, nMfcc, firstMfcc);
    }
    return nMfcc;

  }
}


// blocksize is size of mspec block (=nBands)
int cMfcc::initTables( long blocksize, int idxc )
{
  int i,m;
  FLOAT_DMEM *_costable = costable[idxc];
  FLOAT_DMEM *_sintable = sintable[idxc];
  
  if (_costable != nullptr) free(_costable);
  _costable = (FLOAT_DMEM *)malloc(sizeof(FLOAT_DMEM)*blocksize*nMfcc);
  if (_costable == nullptr) OUT_OF_MEMORY;

  double fnM = (double)(blocksize);
  for (i=firstMfcc; i <= lastMfcc; i++) {
    double fi = (double)i;
    for (m=0; m<blocksize; m++) {
      _costable[m+(i-firstMfcc)*blocksize] = (FLOAT_DMEM)cos((double)M_PI*(fi/fnM) * ( (double)(m) + (double)0.5) );
    }
  }

  if (_sintable != nullptr) free(_sintable);
  _sintable = (FLOAT_DMEM *)malloc(sizeof(FLOAT_DMEM)*nMfcc);
  if (_sintable == nullptr) OUT_OF_MEMORY;

  if (cepLifter > 0.0) {
    for (i=firstMfcc; i <= lastMfcc; i++) {
      _sintable[i-firstMfcc] = ((FLOAT_DMEM)1.0 + cepLifter/(FLOAT_DMEM)2.0 * sin((FLOAT_DMEM)M_PI*((FLOAT_DMEM)(i))/cepLifter));
    }
  } else {
    for (i=firstMfcc; i <= lastMfcc; i++) {
      _sintable[i-firstMfcc] = 1.0;
    }
  }
  costable[idxc] = _costable;
  sintable[idxc] = _sintable;
  return 1;
}

// idxi=input field index
int cMfcc::processVectorFloat(const FLOAT_DMEM *src, FLOAT_DMEM *dst, long Nsrc, long Ndst, int idxi) 
{
  int i,m;
  idxi = getFconf(idxi);
  FLOAT_DMEM *_costable = costable[idxi];
  FLOAT_DMEM *_sintable = sintable[idxi];

  FLOAT_DMEM *_src;
  _src = (FLOAT_DMEM*)malloc(sizeof(FLOAT_DMEM)*Nsrc);
  if (_src==nullptr) OUT_OF_MEMORY;
  
  if (inverse) {
    FLOAT_DMEM factor = (FLOAT_DMEM)sqrt((double)2.0/(double)(nBands));
    if (lastMfcc-firstMfcc+1 != Nsrc) {
      SMILE_IERR(1,"Input dimensionality mismatch! Expected (lastMfcc-firstMfcc+1): %i , actual: %i",lastMfcc-firstMfcc+1,Nsrc);
      return 0;
    }

    // inverse liftering
    for (i=firstMfcc; i <= lastMfcc; i++) {
      int i0 = i-firstMfcc;
      int srcIdx = i0;
      if (htkcompatible && (firstMfcc==0)) {
        // ASSERTION: i == i0
        if (i == 0) {
          srcIdx = lastMfcc;
        }
        else {
          srcIdx = i0 - 1;
        }
      }
      FLOAT_DMEM * inc = _src+srcIdx;
      *inc = src[srcIdx] / (_sintable[i0]);
    }

    // inverse DCT
    for (m=0; m<nBands; m++) {
      FLOAT_DMEM * outc = dst+m;

      * outc = 0.0;
      for (i=firstMfcc; i <= lastMfcc; i++) {
        int i0 = i-firstMfcc;
        int srcIdx = i0;
        if (htkcompatible && (firstMfcc==0)) {
          // ASSERTION: i == i0
          if (i == 0) {
             srcIdx = lastMfcc;
          }
          else {
             srcIdx = i0 - 1;
          }
        }
        //Rprintf("i = %d, i0 = %d, srcIdx = %d\n", i, i0, srcIdx);
        FLOAT_DMEM correctionfactor = 1.0;
        if (i==0) correctionfactor = (FLOAT_DMEM) ( 0.5f );
        *outc += _src[srcIdx] * _costable[m + i0*nBands] * correctionfactor * factor;
      }

      // inverse Log
      if (doLog_) {
        *outc = (FLOAT_DMEM)( exp(*outc) );
      }
    } 
  } else {

  // compute log mel spectrum
  if (doLog_) {
    for (i = 0; i < Nsrc; i++) {
      if (src[i] < melfloor) _src[i] = log(melfloor);
      else _src[i] = (FLOAT_DMEM)log(src[i]);
    }
  } else {
    for (i = 0; i < Nsrc; i++) {
      _src[i] = (FLOAT_DMEM)src[i];
    }
  }

  // compute dct of mel data & do cepstral liftering:
  FLOAT_DMEM factor = (FLOAT_DMEM)sqrt((double)2.0/(double)(Nsrc));
  for (i=firstMfcc; i <= lastMfcc; i++) {
    int i0 = i-firstMfcc;
    FLOAT_DMEM * outc = dst+i0;  // = outp + (i-obj->firstMFCC);
    if (htkcompatible && (firstMfcc==0)) {
      if (i==lastMfcc) { i0 = 0; }
      else { i0 += 1; }
    }
    *outc = 0.0;
    for (m=0; m<Nsrc; m++) {
      *outc += _src[m] * _costable[m + i0*Nsrc];
    }
    if (printDctBaseFunctions) {
      Rprintf("base_mfcc_%i = [", i);
      for (m=0; m<Nsrc-1; m++) {
        Rprintf("%e ", _costable[m + i0*Nsrc]);
      }
      Rprintf("%e];\n", _costable[m + i0*Nsrc]);
    }
    //*outc *= factor;   // use this line, if you want unliftered mfcc
    // do cepstral liftering:
    *outc *= _sintable[i0] * factor;
  }
  if (printDctBaseFunctions) {
      printDctBaseFunctions = 0;
  }

  }
  free(_src);
  return 1;
}

cMfcc::~cMfcc()
{
  multiConfFree(costable);
  multiConfFree(sintable);
}

