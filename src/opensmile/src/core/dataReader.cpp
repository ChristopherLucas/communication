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


/*  openSMILE component: dataReader */


#include <core/dataReader.hpp>

#define MODULE "cDataReader"

SMILECOMPONENT_STATICS(cDataReader)

SMILECOMPONENT_REGCOMP(cDataReader)
{
  SMILECOMPONENT_REGCOMP_INIT
  scname = COMPONENT_NAME_CDATAREADER;
  sdescription = COMPONENT_DESCRIPTION_CDATAREADER;

  // configure your component's configType:
  SMILECOMPONENT_CREATE_CONFIGTYPE
    //(if set, this sets the dmInstance option in both 'reader' and 'writer' subcomponents. This option is provided for simplification only, it has no other function than setting both reader.dmInstance and writer.dmInstance.
  ct->setField("dmInstance", "The name of the dataMemory instance this reader should connect to.", "dataMemory",0,0);
  ct->makeMandatory(ct->setField("dmLevel", "The level in the data memory instance specified by 'dmInstance' which to read from. If this array element contains more than one element, this reader will read data from multiple input levels, and concattenate the data to generate a single frame/vector. It is a good practice to have unique field names in all levels that you wish to concatenate. Note: If reading from multiple levels, the reader can only return a successfully read frame, if data is available for reading on all input levels. If data is missing on one level, the reader cannot output data, even if data is present on the other levels.", (char *)nullptr, ARRAY_TYPE));
  ct->setField("forceAsyncMerge", "1/0 = yes/no : force framewise merging of levels with differing frame period, if multiple levels are specified in dmLevel", 0);
  ct->setField("errorOnFullInputIncomplete", "1/0 = yes/no : 1 = abort with an error if full input matrix reading is activated (frameSize=0 & frameStep=0 => frameMode=full) and beginning of matrix (curR) is not 0, (if this option is set to 0, only a warning is shown)", 1);

  SMILECOMPONENT_IFNOTREGAGAIN( {} )
  
  SMILECOMPONENT_MAKEINFO_NODMEM(cDataReader);
}

SMILECOMPONENT_CREATE(cDataReader)

///------------------------------------ dynmaic part:

cDataReader::cDataReader(const char *_name) : cSmileComponent(_name),
  dm(nullptr),
  dmInstName(nullptr),
  dmLevel(nullptr),
  level(nullptr),
  rdId(nullptr),  
  forceAsyncMerge(0),
  errorOnFullInputIncomplete(1),
  curR(0),  
  V(nullptr),
  m(nullptr),  
  stepM(1),
  lengthM(1),
  ignMisBegM(0),
  stepM_sec(-1.0),
  lengthM_sec(-1.0),
  ignMisBegM_sec(0.0),
  Lf(nullptr),
  Le(nullptr),
  fToL(nullptr),  
  eToL(nullptr),
  myfmeta(nullptr),
  myLcfg(nullptr)
{

}

void cDataReader::fetchConfig() {
  // get name of dataMemory instance to connect to
  dmInstName = getStr("dmInstance");
  if (dmInstName == nullptr) COMP_ERR("fetchConfig: getStr(dmInstance) returned nullptr! missing option in config file?");
  // now get read level information:
  nLevels = getArraySize("dmLevel");
  SMILE_IDBG(2,"'%s': reading from %i input levels",getInstName(),nLevels);
  
  forceAsyncMerge = getInt("forceAsyncMerge");
  SMILE_IDBG(2,"forceAsyncMerge = %i",forceAsyncMerge);

  errorOnFullInputIncomplete = getInt("errorOnFullInputIncomplete");
  SMILE_IDBG(2,"errorOnFullInputIncomplete = %i",errorOnFullInputIncomplete);

  int i;
  // XXX : read array of levels...
  if (nLevels > 0) {
    dmLevel = (const char **)calloc(1,sizeof(const char*)*nLevels);
    level = (int *)calloc(1,sizeof(int)*nLevels);
    rdId = (int *)calloc(1,sizeof(int)*nLevels);

    if (dmLevel==nullptr) OUT_OF_MEMORY;
    for (i=0; i<nLevels; i++) {
      dmLevel[i] = getStr_f(myvprint("dmLevel[%i]",i));
      if (dmLevel[i] == nullptr) COMP_ERR("fetchConfig: getStr(dmLevel[%i]) returned nullptr! missing option in config file?",i);
      SMILE_IDBG(2,"'%s': input level %i = '%s'",getInstName(),i,dmLevel[i]);
      rdId[i] = -1;
    }
  }
}

int cDataReader::myRegisterInstance(int *runMe)
{
  const char * tn = getComponentInstanceType(dmInstName);
  if (tn == nullptr) {
    SMILE_IWRN(4,"cannot yet find dataMemory component '%s'!",dmInstName);
    return 0;
  }
  if (!strcmp(tn, COMPONENT_NAME_CDATAMEMORY)) {
    dm = (cDataMemory *)getComponentInstance(dmInstName);
    if (dm == nullptr) {
      SMILE_IERR(1,"dataMemory instance dmInstance='%s' was not found in componentManager!",dmInstName);
//      COMP_ERR("failed to register!");
      return 0;
    }
  } else { // not datamemory type..
    if (dm == nullptr) {
      SMILE_IERR(1,"dmInstance='%s' -> not of type %s (dataMemory)!",dmInstName,COMPONENT_NAME_CDATAMEMORY);
//      COMP_ERR("failed to register!");
      return 0;
    }
  }

  int i;
  for (i=0; i<nLevels; i++) { 
    dm->registerReadRequest(dmLevel[i],getInstName());
  }

  return 1;
}

int cDataReader::myConfigureInstance()
{
  int i;

  for (i=0; i<nLevels; i++) {
    // find our read levels... the dataWriters should create them in their configure method
    level[i] = dm->findLevel(dmLevel[i]);
    if (level[i] < 0) {
      SMILE_IDBG(3,"level='%s' not yet available, waiting!",dmLevel[i]);
      return 0;
    }
  }

  for (i=0; i<nLevels; i++) {
    // setup the reader blocksize and get read level config
    const sDmLevelConfig * c;
    if (lengthM_sec >= 0.0) {
      c = dm->queryReadConfig( level[i], lengthM_sec );
    } else {
      long tmp = (long)lengthM;
      if (tmp < 1) tmp = 1;
      c = dm->queryReadConfig( level[i], tmp );
    }
    // TODO: merge config from various levels....

    //...
    if ((myLcfg==nullptr)&&(c!=nullptr)) myLcfg = new sDmLevelConfig(*c);
  }
  
  if (myLcfg == nullptr) {
    SMILE_IERR(1,"reader level config could not be set in myConfigureInstance for an unknown reason!");
    return 0;
  }

  // now update stepM and lengthM from stepM_frames and lengthM_frames:
  if (stepM < 0) {
    if (myLcfg->T != 0.0) {
      stepM = (long)round( stepM_sec / myLcfg->T );
      ignMisBegM = (long)round( ignMisBegM_sec / myLcfg->T );
    } else {
      stepM = (long)round( stepM_sec );
      ignMisBegM = (long)round( ignMisBegM_sec );
    }
    curR = ignMisBegM;
  }

  if (lengthM < 0) {
    if (myLcfg->T != 0.0) {
      lengthM = (long)round( lengthM_sec / myLcfg->T );
    } else {
      lengthM = (long)round( lengthM_sec );
    }
  }

  return 1;
}

int cDataReader::myFinaliseInstance()
{
  int i,j;
  double lT = -1.0;
  long lN = 0;
  long lNf = 0;
  int ldtype = -1;

  for (i = 0; i < nLevels; i++) {
    // find multiple levels, register multiple readers...
    if (!dm->namesAreSet(level[i])) {
      SMILE_IDBG(3,"finaliseInstance: names in input level '%s' are not yet set... waiting.",
          dmLevel[i]);
      return 0;
    }
  }

  if (Lf == nullptr)
    Lf = (int*)calloc(1,sizeof(int)*(nLevels+1));
  if (Le == nullptr)
    Le = (int*)calloc(1,sizeof(int)*(nLevels+1));
  int * bufSizes = (int *)calloc(1, sizeof(int) * (nLevels + 1));
  int isRb = -1;
  int growDyn = -1;
  int bufSizeMax = 0;

  for (i = 0; i < nLevels; i++) {
    if (rdId[i] == -1) {
      rdId[i] = dm->registerReader(level[i]);
      SMILE_IDBG(2, "registered reader level[i=%i]=%i ('%s'), rdId=%i",
          i, level[i], dm->getLevelName(level[i]), rdId[i]);
    }
    
    const sDmLevelConfig *c = dm->getLevelConfig(level[i]);
    if (c != nullptr) {
      // check bufsizes, growdyn and rb for consistency.
      // if these are not consistent this might cause hidden problems, i.e. blocking the processing chain
      bufSizes[i] = c->nT;
      if (c->nT > bufSizeMax)
        bufSizeMax = c->nT;
      if (isRb == -1)
        isRb = c->isRb;
      if (growDyn == -1)
        growDyn = c->growDyn;
      if (growDyn != c->growDyn) {
        SMILE_IWRN(2, "Inconsistency in input level parameters. growDyn on first (#1) input = %i, growDyn on input # %i (%s) = %i. This might cause the processing to hang unpredictably or cause incomplete processing.",
            growDyn, i + 1, c->name, c->growDyn);
      }
      if (isRb != c->isRb) {
        SMILE_IWRN(2, "Inconsistency in input level parameters. isRb on first (#1) input = %i, isRb on input # %i (%s) = %i. This might cause the processing to hang unpredictably or cause incomplete processing.",
            isRb, i + 1, c->name, c->isRb);
      }

      // check if all levels have the same period!!
      // (do not proceed unless forceAsyncMerge option is given in config!)
      if (lT == -1.0)
        lT = c->T;
      else if ((c->T != lT) && (fabs(c->T - lT) > 10e-10) && (!forceAsyncMerge)) {
        SMILE_IERR(1, "frame period mismatch among input levels! '%s':%e <> '%s':%e",
            dm->getLevelName(level[i]), c->T, dm->getLevelName(level[0]), lT);
        return 0;
      }
      if (ldtype==-1)
        ldtype = c->type;
      else if (ldtype != c->type) {
        SMILE_IERR(1, "dataType mismatch among input levels (conversion is not yet supported!) '%s':%i <> '%s':%i",
            dm->getLevelName(level[i]), c->type, dm->getLevelName(level[0]), ldtype);
        return 0;
      }

    } else { 
      SMILE_IERR(1,"can't get config for level '%s' (i=%i level[i]=%i)",dmLevel[i],i,level[i]); 
      return 0; 
    }
    
    // BUILD a field -> level map for getFieldName...
    if (c->fmeta != nullptr) Lf[i] = lNf;
    lNf += c->fmeta->N;
    // BUILD a element -> level map for getElementName...
    Le[i] = lN;
    lN += c->N;

    //SMILE_IDBG(4,"finalised dmLevel[%i]='%s' level[%i]=%i in memory '%s'",getInstName(),i,dmLevel[i],i,level[i],dmInstName);
  }
  Lf[nLevels] = lNf;
  Le[nLevels] = lN;
  
  for (i = 0; i < nLevels; i++) {
    if (bufSizes[i] < bufSizeMax)
      SMILE_IWRN(1, "Mismatch in input level buffer sizes (levelconf.nT). Level #%i has size %i which is smaller than the max. input size of all input levels (%i). This might cause the processing to hang unpredictably or cause incomplete processing.",
          i, bufSizes[i], bufSizeMax);
  }
  free(bufSizes);

  // BUILD a field -> level map for getFieldName...
  // BUILD a element -> level map for getElementName...
  fToL = (int*)calloc(1,sizeof(int)*lNf);
  eToL = (int*)calloc(1,sizeof(int)*lN);
  for (i=0; i<nLevels; i++) {
    for (j=Lf[i]; j<Lf[i+1]; j++) {
      fToL[j] = i;
    }
    for (j=Le[i]; j<Le[i+1]; j++) {
      eToL[j] = i;
    }
  }

  myLcfg->N = lN;
  myLcfg->Nf = lNf;
  myLcfg->type = ldtype;

  int updatefmeta = 0;
  int f = 0;
  if (myfmeta == nullptr) {
    myfmeta = new FrameMetaInfo();
    if (myfmeta == nullptr) OUT_OF_MEMORY;
    myfmeta->N = myLcfg->Nf; // assign number of fields (total)
    myfmeta->Ne = myLcfg->N; // assign total number of elements
    myfmeta->field = (FieldMetaInfo *)calloc(1,sizeof(FieldMetaInfo)*(myLcfg->Nf));
    myLcfg->fmeta = myfmeta;
    updatefmeta = 1;
  }
  
  for (i=0; i<nLevels; i++) {
    const sDmLevelConfig *c = dm->getLevelConfig(level[i]);
    if (c!=nullptr) {
      // update myfmeta (*c should contain names by now)
      if (updatefmeta) {
        const FrameMetaInfo * fm = c->fmeta;
        if (fm != nullptr) {
          int j;
          for (j=0; j<fm->N; j++) {
            // now copy each field  from corresponding frame
            myfmeta->field[f++].copyFrom(fm->field+j);
          }
        }
      }
    }
  }

  //----- DONE: ???
  // todo .. MANUALLY update other values in myLcfg, e.g. blocksizeWriter, etc...

  // TODO:: myLcfg->fmeta = myfmeta...  : DONE: this is handled when the first frame arrives and a concattenated fmeta is built
  // HOWEVER, it would be better to concattenate the fmeta at this point.....
  
  return 1;
}

long cDataReader::getMinR()
{ 
  // NOTE: "minimum index that is readable", thus we must take the MAX among all input levels!!
  long minR = dm->getMinR(level[0]);
  int i;
  for (i=1; i<nLevels; i++) {
    long tmp = dm->getMinR(level[i]);
    if (tmp > minR) minR = tmp;
  }
  return minR;
}

void cDataReader::catchupCurR(long _curR)
{
  int i;
  for (i=0; i<nLevels; i++) {
    //printf("catchup rdid %i in '%s' '%s'\n",rdId[i],getInstName(),dmLevel[i]);
    dm->catchupCurR(level[i],rdId[i],_curR);
  }
}

//-----
cVector * cDataReader::getFrame(long vIdx, int special, int privateVec, int *result)
{
         // XXX : get multiple frames, concat them
         // PROBLEM::: only return success, if all frames were read successfully
         //             if not all frames were read successfully, UNDO changes to write/read counters!!
         // THUS::=> use a checkRead() function to see if data is available!!
  long i;
  int r=1;
  cVector *_V = nullptr;
  if (!privateVec) _V = V;
  if (result != nullptr) *result = 0;

  if (nLevels > 1) {
  
    for (i=0; i<nLevels; i++) {
      int myResult=0;
      r &= dm->checkRead(level[i],vIdx,special,rdId[i], 1, &myResult);
      if (result != nullptr) *result |= myResult;
    }
    if (r) {
      if (_V==nullptr) _V = new cVector(myLcfg->N,myLcfg->type);
      int fmetaUpdate = 0;
      if (myfmeta == nullptr) { 
        fmetaUpdate = 1;
        myfmeta = new FrameMetaInfo();
        if (myfmeta == nullptr) OUT_OF_MEMORY;
        myfmeta->N = myLcfg->Nf; // assign number of fields (total)
        myfmeta->field = (FieldMetaInfo *)calloc(1,sizeof(FieldMetaInfo)*(myLcfg->Nf));
        myLcfg->fmeta = myfmeta;
      }
      long e=0; 
      long f=0;
      for (i=0; i<nLevels; i++) {
        //        printf("rdid i = %i\n",rdId[i]);
        int myResult=0;
        cVector *f2 = dm->getFrame(level[i],vIdx, special, rdId[i], &myResult);
        if (result != nullptr) *result |= myResult;

        if (f2 != nullptr) {
          // copy data from f2 into V at the correct position
          if (f2->type == DMEM_FLOAT) {
            memcpy( _V->dataF+e , f2->dataF, f2->N*sizeof(FLOAT_DMEM) );
            
            //            for (n=0; n<f2->N; n++)
            //              V->dataF[e++] = f2->dataF[n];
          } else if (f2->type == DMEM_INT) {
            memcpy( _V->dataI+e , f2->dataI, f2->N*sizeof(INT_DMEM) );
            
            //            for (n=0; n<f2->N; n++)
            //              V->dataI[e++] = f2->dataI[n];
          }
          e += f2->N;
          // TODO: implement conversion from dataI <-> dataF


          if (i==0) _V->tmetaClone(f2->tmeta); // TODO: change this for asynchronous levels...

          //concat fmeta!!
          if (fmetaUpdate) {
            int j;
            for (j=0; j<f2->fmeta->N; j++) {
              // now copy each field  from corresponding frame
              myfmeta->field[f++].copyFrom(f2->fmeta->field+j);
            }
            //f += f2->fmeta->N;
          }
          _V->fmeta = myfmeta;

          delete f2;
        } else {
          SMILE_ERR(1,"no data was read from one of multiple input levels, this is a BUG! checkRead <-> getFrame ! a bogus data vector will now be returned!");
        }
      }

      if (!privateVec) V=_V;
      //if ((_V != nullptr)&&(vIdx>curR)) curR=vIdx;
      return _V;
    } else {
      return nullptr;
    }
  
  } else {
    // TODO: speed-up by passing our V object to dm->getFrame, which fills it and does not have to reallocate an object!
    cVector *f2 = dm->getFrame(level[0],vIdx, special, rdId[0], result);
    if ((f2 != nullptr)&&(!privateVec)) {
      if (V != nullptr) delete V;
      V = f2;
    }
    //if ((f2 != nullptr)&&(vIdx>curR)) curR=vIdx;
    return f2;
  }
}

cMatrix * cDataReader::getMatrix(long vIdx, long length, int special, int privateVec) // vIdx: start index of matrix (absolute)
{
         // XXX TODO: get multiple frames, concat them
         // PROBLEM::: only return success, if all frames were read successfully
         //             if not all frames were read successfully, UNDO changes to write/read counters!!
         // THUS::=> use a checkRead() function to see if data is available!!

  long i,n;
  int r=1;
  cMatrix *my_m = nullptr;
  if (!privateVec) my_m=m;
  
  if (nLevels > 1) {

    for (i=0; i<nLevels; i++) {
      r &= dm->checkRead(level[i],vIdx,special,rdId[i],length);
    }
    if (r) {
      if (my_m!=nullptr) {
        if (length != my_m->nT) {
          delete my_m;
          my_m=nullptr;
        }
      }
      if (my_m == nullptr) my_m = new cMatrix(myLcfg->N,length,myLcfg->type);
      
      int fmetaUpdate = 0;
      if (myfmeta == nullptr) { 
        fmetaUpdate = 1;
        myfmeta = new FrameMetaInfo();
        if (myfmeta == nullptr) OUT_OF_MEMORY;
        myfmeta->N = myLcfg->Nf; // assign number of fields (total)
        myfmeta->field = (FieldMetaInfo *)calloc(1,sizeof(FieldMetaInfo)*(myLcfg->Nf));
        myLcfg->fmeta = myfmeta;
      }

      FLOAT_DMEM*df = my_m->dataF;
      INT_DMEM*di = my_m->dataI;
      long N = myLcfg->N;
      long f=0;
      long minlen = length;
      for (i=0; i<nLevels; i++) {

        cMatrix *m2 = dm->getMatrix(level[i],vIdx,vIdx+length, special, rdId[i]);
        if (m2 != nullptr) {
          if (m2->nT < minlen) { minlen = m2->nT; }
          // copy data from f2 into V at the correct position
          if (m2->type == DMEM_FLOAT) {
            for (n=0; n<minlen /*length*/; n++)
              memcpy( df+(N*n) , m2->dataF + n*(m2->N), m2->N*sizeof(FLOAT_DMEM) );
            df += m2->N;
          } else if (m2->type == DMEM_INT) {
            for (n=0; n<minlen /*length*/; n++)
              memcpy( di+(N*n) , m2->dataI + n*(m2->N), m2->N*sizeof(INT_DMEM) );
            di += m2->N;
          }
          // TODO: implement conversion from dataI <-> dataF

          if (i==0) my_m->tmetaClone(m2->tmeta); // TODO: change this for asynchronous levels...

          //concat fmeta!!
            if (fmetaUpdate) {
              int j;
              for (j=0; j<m2->fmeta->N; j++) {
                // now copy each field  from corresponding frame
                myfmeta->field[f++].copyFrom(m2->fmeta->field+j);
              }
              //f += f2->fmeta->N;
            }
            my_m->fmeta = myfmeta;

          delete m2;
        }
      }
      if (minlen < length) {
        // TODO: if the matrix size increases again, we cannot increase! Introduce cMatrix->nAlloc variable?
        my_m->nT = minlen;
      }

      if (!privateVec) m=my_m;
      return m;
    } else {
      return nullptr;
    }

  } else {
    cMatrix *m2 = dm->getMatrix(level[0],vIdx,vIdx+length, special, rdId[0]);
    if ((m2 != nullptr)&&(!privateVec)) {
      if (m != nullptr) delete m;
      m = m2;
      // ???:
      //if (vIdx+length > curR) curR = vIdx+length;
    }
    return m2;
  }
}

// relative: (vIdxRelE is positive and indicates the number of frames to go back from the last read frame)
// noInc: 1=do not increase current read counter (DEFAULT is to INCREASE READ COUNTER!)
cVector * cDataReader::getFrameRel(long vIdxRelE, int privateVec, int noInc, int *result)
{
  cVector * ret = getFrame(curR-vIdxRelE,-1,privateVec,result);
  if ((!noInc)&&((ret!=nullptr)||(curR-vIdxRelE < 0))) { curR++; }
  return ret;
}

// vIdxRelE: end of matrix relative to end of data
cMatrix * cDataReader::getMatrixRel(long vIdxRelE, long length, int privateVec)
{
  return getMatrix(curR-vIdxRelE-length, curR-vIdxRelE, -1, privateVec);
}  

// sequential
cVector * cDataReader::getNextFrame(int privateVec, int *result)
{
  cVector *ret = getFrame(curR,-1,privateVec,result);
  if ((ret != nullptr)||(curR < 0)) curR++;
  return ret;
}

cMatrix * cDataReader::getNextMatrix(int privateVec, int readToEnd, int special)
{
  if (stepM == 0 || readToEnd == 1) { // read complete input...
    if (isEOI() && EOIlevelIsMatch()) { 
     // EOIlevelIsMatch: need to query EOI level here to avoid components running before they should in multi-EOI iterations
      int i;
      long fl = -1;  // fl = length of input
      for (i=0; i<nLevels; i++) {
        long tmp = dm->getNAvail(level[i],rdId[i]);
        if (fl==-1) fl = tmp;
        else if (tmp < fl) fl = tmp;
      }
      if ((curR==0||readToEnd==1)&&(fl > 0)) {
        /* TODO: curR will not be set correctly be getMatrix for arbitrary reads.. */
        cMatrix *ret = getMatrix(curR,fl,-1,privateVec);
        long tmpR = dm->getMinR(level[0]);
        //		printf("avail: %i  free: %i\n ",dm->getNAvail(level[0]), dm->getNFree(level[0]));
        //		printf("curW: %i  curR(int): %i   curR: \n ",dm->getCurW(level[0]),curR);
        SMILE_IDBG(3,"fullinput: read %i frames (idx %i -> %i).",fl,tmpR,tmpR+fl);

        if (((tmpR > 0)||((myLcfg->growDyn==0)&&(myLcfg->nT < fl)))&&(ret!=nullptr)) {
          if (errorOnFullInputIncomplete) SMILE_IERR(1,"reading of full input is incomplete: read %i frames (idx %i -> %i). start index should be zero! you are having a problem with your buffersizes (%i)!",fl,tmpR,tmpR+fl,myLcfg->nT)
          else SMILE_IWRN(2,"reading of full input is incomplete: read %i frames (idx %i -> %i). start index should be zero! you are having a problem with your buffersizes (%i)!",fl,tmpR,tmpR+fl,myLcfg->nT)
        }
        if (ret != nullptr) { curR += fl; }
        return ret;
      } else { return nullptr; }
    } else { return nullptr; }
  } else {
    cMatrix *ret = getMatrix(curR, lengthM, special, privateVec);
    if (ret != nullptr) curR += stepM;
    return ret;
  }
}

long cDataReader::getNAvail()
{
  long fl = -1; // fl = nAvailable
  int i;
  for (i=0; i<nLevels; i++) {
    long tmp = dm->getNAvail(level[i],rdId[i]);
    if (fl==-1) fl = tmp;
    else if (tmp < fl) fl = tmp;
  }
  return fl;
}

long cDataReader::getNFree()
{
  long fl = -1;  // fl = nFree
  int i;
  for (i=0; i<nLevels; i++) {
    long tmp = dm->getNFree(level[i],rdId[i]);
    if (fl==-1) fl = tmp;
    else if (tmp < fl) fl = tmp;
  }
  return fl;
}

// in order to report blocksize to dataMemory correctly, this should be called before configure()
int cDataReader::setupSequentialMatrixReading(long step, long length, long ignoreMissingBegin)
{
  if ((step < 0) || (length < 0)) {
    SMILE_IERR(2,"step (%i) OR length (%i) < 0 in setupSequentialMatrixReading (frames)",step,length);
    return 0;
  }

  stepM = step;
  lengthM = length;
  if ((length<=0)||(step<=0)) { stepM=0; lengthM=0; }
  stepM_sec = -1.0;
  lengthM_sec = -1.0;
  curR = ignMisBegM = ignoreMissingBegin;

  if (isConfigured()) {
    // we must update the blocksize in DM!
    updateBlocksize(lengthM+stepM);
  }
  return 1;
}

// since the period of the reader level is not available before configure(), we must provide both methods, 
// one for setting the parameters in frames, and one for setting them seconds, as this:
int cDataReader::setupSequentialMatrixReading(double step, double length, double ignoreMissingBegin)
{
  if ((step < 0.0) || (length < 0.0)) {
    SMILE_IERR(2,"step (%f) OR length (%f) < 0.0 in setupSequentialMatrixReading (seconds)",step,length);
    return 0;
  }
  stepM_sec = step;
  lengthM_sec = length;
  ignMisBegM_sec = ignoreMissingBegin;
  if ((length<=0.0)||(step<=0.0)) { stepM_sec=0.0; lengthM_sec=0.0; }
  stepM = -1;
  lengthM = -1;
  // this is handled in myConfigureInstance now...
  //curR = ignMisBegM = ignoreMissingBegin;

  if (isConfigured()) {
    // we must update the blocksize in DM and recompute stepM, lengthM and curR
    updateBlocksizeSec(lengthM_sec+stepM_sec);  // WAS -> updateBlocksize() !! why??

    if (myLcfg->T != 0.0) {
      stepM = (long)round( stepM_sec / myLcfg->T );
      ignMisBegM = (long)round( ignMisBegM_sec / myLcfg->T );
      lengthM = (long)round( lengthM_sec / myLcfg->T );
    } else {
      stepM = (long)round( stepM_sec );
      ignMisBegM = (long)round( ignMisBegM_sec );
      lengthM = (long)round( lengthM_sec );
    }
    curR = ignMisBegM;
  }

  return 1;
}


cDataReader::~cDataReader() {
  if (V!=nullptr) delete V;
  if (m!=nullptr) delete m;
  if (dmLevel!=nullptr) free(dmLevel);
  if (rdId != nullptr) free(rdId);
  if (level!=nullptr)  free(level);
  if (Lf!=nullptr) free(Lf);
  if (Le!=nullptr) free(Le);
  if (fToL!=nullptr) free(fToL);
  if (eToL!=nullptr) free(eToL);
  if (myfmeta!=nullptr) delete myfmeta;
  if (myLcfg != nullptr) delete myLcfg;
}
