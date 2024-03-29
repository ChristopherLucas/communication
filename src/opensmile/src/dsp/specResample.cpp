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


/*  openSMILE component: resmapler


*/

// TODO: when upsampling we need a post lp filter (time domain) to remove hf artefacts


#include <dsp/specResample.hpp>

#define MODULE "cSpecResample"


SMILECOMPONENT_STATICS(cSpecResample)

SMILECOMPONENT_REGCOMP(cSpecResample)
{
  SMILECOMPONENT_REGCOMP_INIT
  scname = COMPONENT_NAME_CSPECRESAMPLE;
  sdescription = COMPONENT_DESCRIPTION_CSPECRESAMPLE;

  // we inherit cVectorProcessor configType and extend it:
  SMILECOMPONENT_INHERIT_CONFIGTYPE("cVectorProcessor")

  // if the inherited config type was found, we register our configuration variables
  SMILECOMPONENT_IFNOTREGAGAIN( {} // <- this is only to avoid compiler warnings...
    // name append has a special role: it is defined in cDataProcessor, and can be overwritten here:
	  // if you set description to nullptr, the existing description will be used, thus the following call can
  	// be used to update the default value:
    //ct->setField("nameAppend",nullptr,"processed");

    ct->setField("targetFs","The target sampling frequency in Hz",16000);  
    ct->setField("resampleRatio","Specifies a fixed resample ratio a (a=fsNew/fsCurrent). If set, this overrides targetFs",1.0,0,0);

    ct->setField("inputFieldPartial","The name of the input field to search for. (nullptr (default): use full input vector)",(const char*)nullptr);
    ct->setField("processArrayFields",nullptr,0); // TODO? support array field processing...
  )

  // The configType gets automatically registered with the config manger by the SMILECOMPONENT_IFNOTREGAGAIN macro
  
  // we now create out sComponentInfo, including name, description, success status, etc. and return that
  SMILECOMPONENT_MAKEINFO(cSpecResample);
}

SMILECOMPONENT_CREATE(cSpecResample)

//-----

cSpecResample::cSpecResample(const char *_name) :
  cVectorProcessor(_name),
  inData(nullptr),
  resampleRatio(1.0),
  inputFieldPartial(nullptr),
  antiAlias(1),
  dftWork(nullptr)
  //costable(nullptr),sintable(nullptr)
{

}

void cSpecResample::fetchConfig()
{
  cVectorProcessor::fetchConfig();

  if (isSet("resampleRatio")) {
    resampleRatio = getDouble("resampleRatio");
    if (resampleRatio <= 0.0) {
      SMILE_IERR(1,"invalid resampling ratio (%f) ! must be > 0.0",resampleRatio);
      resampleRatio = 1.0;
    }
    SMILE_IDBG(2,"resampleRatio = '%s'",resampleRatio);
  } else {
    targetFs = getDouble("targetFs");
    if (targetFs <= 0.0) {
      SMILE_IERR(1,"invalid target sampling frequency (targetFs=%f) ! must be > 0.0",targetFs);
      targetFs = 1.0;
    }
    SMILE_IDBG(2,"targetFs = '%s'",targetFs);
    resampleRatio = -1.0;
  }

  inputFieldPartial = getStr("inputFieldPartial");
}

int cSpecResample::configureWriter(sDmLevelConfig &c)
{
  if ((c.lastFrameSizeSec != c.basePeriod)&&(c.lastFrameSizeSec > 0.0))
    c.frameSizeSec = c.lastFrameSizeSec;

  double bT = (double)(c.basePeriod);
  if (bT > 0.0) sr = 1.0/bT;
  else {
    SMILE_IERR(1,"unable to determine sample rate of input! basePeriod <= 0.0 (=%f)!",bT);
    sr = 1.0;
  }

  /* compute resampling parameters: */
  if (resampleRatio == -1.0) { // convert targetFs
    resampleRatio = targetFs/sr;
    SMILE_IDBG(2,"resampleRatio (computed) = %f",resampleRatio);
  } else {
    // compute targetFs from resampling ratio
    targetFs = resampleRatio * sr;
  }

  c.basePeriod = 1.0/targetFs;
  return 1;
}

int cSpecResample::setupNewNames(long nEl)
{
  const sDmLevelConfig *c = reader_->getLevelConfig();
  fsSec = (double)(c->frameSizeSec);
  double lastFsSec = (double)(c->lastFrameSizeSec);
  double bT = (double)(c->basePeriod);
  if (bT > 0.0) sr = 1.0/bT;
  else {
    SMILE_IERR(1,"unable to determine sample rate of input! basePeriod <= 0.0 (=%f)!",bT);
    sr = 1.0;
  }

  if (inputFieldPartial != nullptr) {
    findInputField(inputFieldPartial, 0, nEl);
  } else {
    nInput_=nEl;
    inputStart_ = 0;
  }
  _Nin = nInput_;

  
  // we must round the resampling ratio to the closest integer frame size for the target frames to avoid discontinuities
  
  // detect zero-padded fft input and adjust actual output
  double nd;
  if ((fsSec != lastFsSec)&&(lastFsSec != 0.0)&&(lastFsSec!=bT)) {
    double _Nout0 = round((double)_Nin * resampleRatio * lastFsSec/fsSec);
    double newRatio = _Nout0 / ((double)_Nin * (lastFsSec/fsSec));
    _Nout = (long)_Nout0;
    if (newRatio != resampleRatio) {
      targetFs = sr*newRatio;
      SMILE_IMSG(2,"adjusting resampleRatio from %f to %f to match next integer frame size! (targetFs* = %f)",resampleRatio,newRatio,targetFs);
      resampleRatio = newRatio;
    }
    nd = (double)_Nin * resampleRatio;
  } else {
    double _Nout0 = round((double)_Nin * resampleRatio);
    double newRatio = _Nout0 / (double)_Nin;
    _Nout = (long)_Nout0;
    if (newRatio != resampleRatio) {
      targetFs = sr*newRatio;
      SMILE_IMSG(2,"adjusting resampleRatio from %f to %f to match next integer frame size! (targetFs* = %f)",resampleRatio,newRatio,targetFs);
      resampleRatio = newRatio;
    }
    nd = _Nout0;
  }

  dftWork = smileDsp_initIrdft(_Nin, _Nout, nd, antiAlias);
  
  //int n=0;
  writer_->addField("resampled",_Nout);
  namesAreSet_ = 1;
  return _Nout;
}


// a derived class should override this method, in order to implement the actual processing
int cSpecResample::processVectorFloat(const FLOAT_DMEM *src, FLOAT_DMEM *dst, long Nsrc, long Ndst, int idxi) // idxi=input field index
{
  //long i,k;
  // we assume we have complex fft as input...

//  if (!getInputFieldData(src,Nsrc,&inData)) return 0;

  smileDsp_irdft(src, dst, dftWork);
  
  return Ndst;
}

cSpecResample::~cSpecResample()
{
  if (inData != nullptr) free(inData);
  smileDsp_freeDftwork(dftWork);
}

