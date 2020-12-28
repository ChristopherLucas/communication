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

Rcpp Data output

*/



#include <iocore/RcppDataSink.hpp>

#define MODULE "cRcppDataSink"

/*Library:
sComponentInfo * registerMe(cConfigManager *_confman) {
  cDataSink::registerComponent(_confman);
}
*/

SMILECOMPONENT_STATICS(cRcppDataSink)

//sComponentInfo * cRcppDataSink::registerComponent(cConfigManager *_confman)
SMILECOMPONENT_REGCOMP(cRcppDataSink)
{
  SMILECOMPONENT_REGCOMP_INIT
  scname = COMPONENT_NAME_CRCPPDATASINK;
  sdescription = COMPONENT_DESCRIPTION_CRCPPDATASINK;

  // we inherit cDataSink configType and extend it:
  SMILECOMPONENT_INHERIT_CONFIGTYPE("cDataSink")

  SMILECOMPONENT_IFNOTREGAGAIN_BEGIN
    ct->setField("delimChar","The column delimiter character to use (usually ',' or ';') (NOTE: use '<space>' or '<tab>' for these special characters respectively)",';');
    ct->setField("lag","output data <lag> frames behind",0,0,0);
    ct->setField("timestamp","1 = print a timestamp attribute for each output frame (1/0 = yes/no)", 1);
    // for compatibility with arffSink
    ct->setField("frameTime","(same as 'timestamp') 1 = print a timestamp attribute for each output frame (1/0 = yes/no)", 1);
    ct->setField("number", "1 = print an instance number (= frameIndex) attribute for each output frame (1/0 = yes/no)", 1);
    // for compatibility with arffSink
    ct->setField("frameIndex", "(same as 'number') 1 = print an instance number (= frameIndex) attribute for each output frame (1/0 = yes/no)", 1);

  SMILECOMPONENT_IFNOTREGAGAIN_END

  SMILECOMPONENT_MAKEINFO(cRcppDataSink);
}

SMILECOMPONENT_CREATE(cRcppDataSink)

//-----

cRcppDataSink::cRcppDataSink(const char *_name) :
  cDataSink(_name),
  instanceName(nullptr),
  instanceBase(nullptr),
  disabledSink_(false), 
  delimChar(';'),
  prname(0),
  setWaveFeaturesCB(nullptr)
{
}

void cRcppDataSink::connectSetWaveFeaturesCB(SetWaveFeaturesCB_Ptr setWaveFeaturesCB_)
{
  setWaveFeaturesCB = setWaveFeaturesCB_;
}

void cRcppDataSink::fetchConfig()
{
  cDataSink::fetchConfig();
  
  delimChar = getChar("delimChar");
  lag = getInt("lag");

  number = getInt("number");
  if (isSet("frameIndex")) {
    number = getInt("frameIndex");
  }
  if (number) { SMILE_IDBG(3,"printing of frameIndex (number) to file is enabled"); }

  timestamp = getInt("timestamp");
  if (isSet("frameTime")) {
    timestamp = getInt("frameTime");
  }
  if (timestamp) { SMILE_IDBG(3,"printing timestamp attribute (index 1) enabled"); }

}

/*
int cRcppDataSink::myConfigureInstance()
{
  int ret=1;
  ret *= cDataSink::myConfigureInstance();
  // ....
  //return ret;
}
*/


int cRcppDataSink::myFinaliseInstance()
{
  if (disabledSink_) {
    return 1;
  }
  int ret = cDataSink::myFinaliseInstance();
  if (ret==0) return 0;

  {
    // write header ....
//    if (prname) {
//      fprintf(filehandle, "name%c", delimChar);
//    }
//    if (number) {
//      fprintf(filehandle, "frameIndex%c", delimChar);
//    }
//    if (timestamp) {
//      fprintf(filehandle, "frameTime%c", delimChar);
//    }

//    long _N = reader_->getLevelN();
//    long i;
//    for(i=0; i<_N-1; i++) {
//      char *tmp = reader_->getElementName(i);
//      fprintf(filehandle, "%s%c",tmp,delimChar);
//      free(tmp);
//    }
//    char *tmp = reader_->getElementName(i);
//    fprintf(filehandle, "%s%s",tmp,NEWLINE);
//    free(tmp);
  }
  
  return ret;
}


int cRcppDataSink::myTick(long long t)
{
  cVector *vec= reader_->getFrameRel(lag);
  //SMILE_IDBG(4, "tick # %i, writing to Rcpp Data (lag=%i) (vec = %ld):",
  //  t, lag, long(vec));
  if (vec == nullptr)
  {
    return 0;
  }
  arma::rowvec row(vec->N);
  for (int i=0; i<vec->N; i++)
  {
    row(i) = static_cast<double>(vec->dataF[i]);
  }
  arma::rowvec time_vec(1);
  time_vec(0) = vec->tmeta->time;
  if(nullptr != setWaveFeaturesCB)
    setWaveFeaturesCB(getCompMan(), row, time_vec);
  //long vi = vec->tmeta->vIdx;
  //double tm = vec->tmeta->time;
//  if (prname == 1) {
//    fprintf(filehandle, "'%s'%c", instanceName, delimChar);
//  } else if (prname == 2) {
//    fprintf(filehandle, "'%s_%i'%c", instanceBase, vi, delimChar);
//  }

//  if (number)
//    fprintf(filehandle,"%i%c",vi,delimChar);
//  if (timestamp)
//    fprintf(filehandle,"%f%c",tm,delimChar);

//  // now print the vector:
//  int i;
//  for (i=0; i<vec->N-1; i++) {
//    // print float as integer if its decimals are zero
//    if (vec->dataF[i] == floor(vec->dataF[i])) {
//      fprintf(filehandle,"%.0f%c",vec->dataF[i],delimChar);
//    } else {
//      fprintf(filehandle,"%e%c",vec->dataF[i],delimChar);
//    }
//  }
//  if (vec->dataF[i] == floor(vec->dataF[i])) {
//    fprintf(filehandle,"%0.f%s",vec->dataF[i],NEWLINE);
//  } else {
//    fprintf(filehandle,"%e%s",vec->dataF[i],NEWLINE);
//  }
//  if (flush) {
//    fflush(filehandle);
//  }
  nWritten_++;

  // tick success
  return 1;
}


cRcppDataSink::~cRcppDataSink()
{

}

