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

functionals meta-component
 
*/


#include <core/dataMemory.hpp>
#include <core/componentManager.hpp>
#include <functionals/functionals.hpp>

#include <math.h>

#define MODULE "cFunctionals"


#define N_BLOCK_ALLOC 50

SMILECOMPONENT_STATICS(cFunctionals)
int cFunctionals::rAcounter=0;
 void resetFunctionalsRaCounter() {
  cFunctionals::rAcounter = 0;
}

SMILECOMPONENT_REGCOMP(cFunctionals)
{
  SMILECOMPONENT_REGCOMP_INIT
  
  scname = COMPONENT_NAME_CFUNCTIONALS;
  sdescription = COMPONENT_DESCRIPTION_CFUNCTIONALS;

  // we inherit cWinToVecProcessor configType and extend it:
//use _compman to find cFunctionalXXXXX component types...
// however, we return rA=1 twice, to allow cFunctionalXXXX to register!
  // add corresponding config sub-types to our config type here....
  SMILECOMPONENT_INHERIT_CONFIGTYPE("cWinToVecProcessor")

  char *funclist=nullptr;

  SMILECOMPONENT_IFNOTREGAGAIN_BEGIN

    if (rAcounter < 2) {
      rA=1;
      rAcounter++;
      SMILECOMPONENT_MAKEINFO(cFunctionals);
    }

    if (_compman != nullptr) {
  
      int nTp = _compman->getNtypes();
      int i,j=0;
      for (i=0; i<nTp; i++) {
        const char * tp = _compman->getComponentType(i,1);
        if (tp!=nullptr) {
          if (!strncmp(tp,"cFunctional",11)&&strncmp(tp,COMPONENT_NAME_CFUNCTIONALS,COMPONENT_NAME_CFUNCTIONALS_LENGTH)) {
             // find beginning "cFunctional" but not our own type (cFunctinals)
            const char *fn = tp+11;
            j++;
            if (funclist != nullptr) {
              char * tmp = funclist;
              funclist = myvprint("%s      %i.\t%s \t\t%s\n",funclist,j,fn,_compman->getComponentDescr(i));
              free(tmp);
            } else {
              funclist = myvprint("     (#) \t(name)    \t\t(description)\n      %i.\t%s \t\t%s\n",j,fn,_compman->getComponentDescr(i));
            }
            // add config type
            char *x = myvprint("functional sub-config of type %s",tp);
            ct->setField(fn,x,sconfman->getTypeObj(tp),NO_ARRAY,DONT_FREE);
            free(x);
          }
        }
      }
    
    } else { // cannot proceed without component manager!
      rA=1;
      SMILECOMPONENT_MAKEINFO(cFunctionals);
    }

    char *x = myvprint("Array that defines the enabled functionals\n    The following functionals are available (sub-components) (Attention: the names are case-SENSITIVE!):\n%s",funclist);
    ct->setField("functionalsEnabled",x,(const char*)nullptr, ARRAY_TYPE);
    free(x);
    free(funclist);

    // set more fields here:
    // NOT possible : ct->setField("globalDuration","output duration of input segment as one value in output (this is useful for variable length inputs)",0);
    ct->setField("nonZeroFuncts","If this is set to 1, functionals are only applied to input values unequal 0. If this is set to 2, functionals are only applied to input values greater than 0.",0);

    ct->setField("functNameAppend","Specifies a string prefix to append to the functional name (which is appended to the input feature name)",(const char*)nullptr);
    ct->setField("masterTimeNorm","This option specifies how all components should normalise times, if they generate output values related to durations. You can change the 'norm' parameter of individual functional components to overwrite this master value. You can choose one of the following normalisation methods: \n   'segment' (or: 'turn') : normalise to the range 0..1, the result indicates relative turn length )\n   'second'  (absolute time in seconds) \n   'frame' (absolute time in number of frames of input level)","segment");
    ct->setField("preserveFields", "If set to 1, preserves the field structure (and metadata, TODO!), of the input vector structure. If set to 0 (default) the output will only have fields with a single element.", 0);

  SMILECOMPONENT_IFNOTREGAGAIN_END

  SMILECOMPONENT_MAKEINFO(cFunctionals);
}

SMILECOMPONENT_CREATE(cFunctionals)

//-----

cFunctionals::cFunctionals(const char *_name) :
  cWinToVecProcessor(_name),
  nFunctTp(0),
  nFunctTpAlloc(0),
  functTp(nullptr),  
  functTpI(nullptr),  
  functI(nullptr),
  functN(nullptr),  
  functObj(nullptr),  
  nonZeroFuncts(0),
  functNameAppend(nullptr),
  timeNorm(TIMENORM_UNDEFINED)
{

}


int cFunctionals::myConfigureInstance()
{
  int i,j;

  nonZeroFuncts = getInt("nonZeroFuncts");
  SMILE_IDBG(2,"nonZeroFuncts = %i \n",nonZeroFuncts);

  if (getInt("preserveFields")) {
    wholeMatrixMode = 1;
    processFieldsInMatrixMode = 1;
    SMILE_IDBG(2, "Whole Matrix Mode enabled with per field processing.");
  }

  functNameAppend = getStr("functNameAppend");
  SMILE_IDBG(2,"functNameAppend = '%s' \n",functNameAppend);

  if (isSet("masterTimeNorm")) {
  const char*Norm = getStr("masterTimeNorm");
  if (Norm != nullptr) {
    if (!strncmp(Norm,"seg",3)) timeNorm=TIMENORM_SEGMENT;
    else if (!strncmp(Norm,"tur",3)) timeNorm=TIMENORM_SEGMENT;
    else if (!strncmp(Norm,"sec",3)) timeNorm=TIMENORM_SECOND;
    else if (!strncmp(Norm,"fra",3)) timeNorm=TIMENORM_FRAME;
  }
  } else {
    timeNorm = TIMENORM_UNDEFINED;
  }

  cComponentManager *_compman = getCompMan();
  if (_compman != nullptr) {
    int nTp = _compman->getNtypes();
    nFunctTp = 0;
    for (i=0; i<nTp; i++) {
      const char * tp = _compman->getComponentType(i,1);
      if (tp!=nullptr) {
        if (!strncmp(tp,"cFunctional",11)&&strcmp(tp,COMPONENT_NAME_CFUNCTIONALS)) {
           // find beginning "cFunctional" but not our own type (cFunctinals)
          const char *fn = tp+11;
          if (nFunctTpAlloc == nFunctTp) { // realloc:
            functTp = (char **)crealloc( functTp, sizeof(char*)*(nFunctTpAlloc+N_BLOCK_ALLOC), nFunctTpAlloc );
            functTpI = (int *)crealloc( functTpI, sizeof(int)*(nFunctTpAlloc+N_BLOCK_ALLOC), nFunctTpAlloc );
            functI = (int *)crealloc( functI, sizeof(int)*(nFunctTpAlloc+N_BLOCK_ALLOC), nFunctTpAlloc );
            functN = (int *)crealloc( functN, sizeof(int)*(nFunctTpAlloc+N_BLOCK_ALLOC), nFunctTpAlloc );
            functObj = (cFunctionalComponent **)crealloc( functObj, sizeof(cFunctionalComponent *)*(nFunctTpAlloc+N_BLOCK_ALLOC), nFunctTpAlloc );
            nFunctTpAlloc += N_BLOCK_ALLOC;
          }
          functTp[nFunctTp] = strdup(fn);
          functTpI[nFunctTp] = i;
          nFunctTp++;
        }
      }
    }
  }
  SMILE_DBG(2,"(inst '%s') found %i cFunctionalXXXX component types.",getInstName(),nFunctTp);

  // fetch enabled functionals list
  nFunctionalsEnabled = getArraySize("functionalsEnabled");
  nFunctValues = 0;
  requireSorted = 0;
  for (i=0; i<nFunctionalsEnabled; i++) {
    const char *fname = getStr_f(myvprint("functionalsEnabled[%i]",i));
    char *tpname = myvprint("cFunctional%s",fname);
    for (j=0; j<nFunctTp; j++) {
      if (!strcmp(functTp[j],fname)) {
        functI[i] = j;
        break;
      }
    }
// TODO: find duplicates in functionalsEnabled Array!!!

    if (j<nFunctTp) {
      // and create corresponding component instances...
        SMILE_IDBG(3,"creating Functional object 'cFunctional%s'.",fname);
        char *_tmp = myvprint("%s.%s",getInstName(),fname);
        cFunctionalComponent * tmp = (cFunctionalComponent *)(_compman->createComponent)(_tmp,tpname);
        free(_tmp);
        if (tmp==nullptr) OUT_OF_MEMORY;
        tmp->setComponentEnvironment(_compman, -1, this);
        tmp->setTimeNorm(timeNorm);
        functN[i] = tmp->getNoutputValues();
        requireSorted += tmp->getRequireSorted();
        nFunctValues += functN[i];
        functObj[i] = tmp;
        //functTp[i]  = strdup(fname);
    } else {
      SMILE_ERR(1,"(inst '%s') Functional object '%s' specified in 'functionalsEnabled' array, however no type 'cFunctional%s' exists!",getInstName(),fname,fname);
      functObj[i] = nullptr;
      functN[i] = 0;
      free(tpname);
      return 0;
      //functTp[i]  = nullptr;
    }
    free(tpname);
  }
  if (requireSorted) { 
    SMILE_IDBG(2,"%i Functional components require sorted data.",requireSorted);
  }

  return cWinToVecProcessor::myConfigureInstance();
}

// We need setupNamesForField if in wholeMatrixMode with ProcessFieldsInMatrixMode

int cFunctionals::setupNamesForElement(int idxi, const char*name, long nEl)
{
  // in a winToVecProcessor , nEl should always be 1!
  int i, j;
  for (i=0; i<nFunctionalsEnabled; i++) {
    if ( (functN[i] > 0) && (functObj[i] != nullptr) ) {
      for (j=0; j<functN[i]; j++) {
        char * newname;
        if (functNameAppend != nullptr) {
          newname = myvprint("%s__%s_%s",name,functNameAppend,functObj[i]->getValueName(j));
        } else {
          newname = myvprint("%s_%s",name,functObj[i]->getValueName(j));
        }

        const FrameMetaInfo * fmeta = reader_->getFrameMetaInfo();
        int ao = 0;
        if (fmeta != nullptr && idxi < fmeta->N) {
          ao = fmeta->field[idxi].arrNameOffset;
        }
        long nElementsOut = functObj[i]->getNumberOfElements(j);
        if (nElementsOut > 0) {
          writer_->addField(newname, nEl * nElementsOut, ao);
          if (fmeta != nullptr && idxi < fmeta->N) {
            // copy metadata (e.g. frequency axis labels...)
            functObj[i]->setFieldMetaData(writer_, fmeta, idxi, nEl * nElementsOut);
/*            double * buf = (double *)malloc(fmeta->field[idxi].infoSize);
            memcpy(buf, fmeta->field[idxi].info, fmeta->field[idxi].infoSize);
            writer_->setFieldInfo(-1, // last field added
                fmeta->field[idxi].dataType, buf,
                fmeta->field[idxi].infoSize);*/
          }
          // TODO: we need to pass arrNameOffset through setupNamesForElement
          // however, this will require modification of the call in winToVecProcessor
          // and thus will require several descendant classes to be updated!
        }
        free(newname);
        j += nElementsOut - 1;
      }
    }
  }
  /*
  if (globalDuration) {
    writer->addField("inputDuration");
  }
  // ^ we cannot do this in this type of component..!
  */
  return nFunctValues * nEl;
}

// this must return the multiplier, i.e. the vector size returned for each input element (e.g. number of functionals, etc.)
int cFunctionals::getMultiplier()
{
  return nFunctValues;
}

int cFunctionals::doProcessMatrix(int idx, cMatrix *rows, FLOAT_DMEM *y, long nOut)
{
  // call doProcess for each row...
  cMatrix *tmpRow = nullptr;
  FLOAT_DMEM *tmpY = (FLOAT_DMEM *)calloc(1, sizeof(FLOAT_DMEM) * nOut);
  FLOAT_DMEM *curY = tmpY;
  long nFuncts = 0;
  if (rows != nullptr) {
    for (int i = 0; i < rows->N; i++) {
      tmpRow = rows->getRow(i, tmpRow);
      long Mu = doProcess(i, tmpRow, curY);
      curY += Mu;
      if (nFuncts == 0) nFuncts = Mu;
    }
    // re-order output vector
    for (int j = 0; j < nFuncts; j++) {
      for (int i = 0; i < rows->N; i++) {
        *y = tmpY[i*nFuncts + j];
        y++;
      }
    }
  }
  if (rows->N * nFuncts != nOut) {
    SMILE_IERR(2, "something is wrong in doProcessMatrix in cFunctionals. expected # outputs %i vs. real num outputs %i (%i * %i)", nOut, rows->N * nFuncts, rows->N, nFuncts);
  }
  free(tmpY);
  if (tmpRow != nullptr) {
    delete tmpRow;
  }
  return rows->N * nFuncts;
}


// idxi is index of input element
// row is the input row
// y is the output vector (part) for the input row
int cFunctionals::doProcess(int idxi, cMatrix *row, FLOAT_DMEM*y)
{
  // copy row to matrix... simple memcpy here
  //  memcpy(y,row->dataF,row->nT*sizeof(FLOAT_DMEM));
  // return the number of components in y!!
  if (row->nT <= 0) {
    SMILE_IWRN(2,"not processing input row of size <= 0 ! (inst '%s')",getInstName());
    return 0;
  }
  SMILE_IDBG(4, "cFunctionals::doProcess (nT = %ld) idxi %i\n", getInstName(), row->nT, idxi);

  long i; int ok=0; long NN=row->nT;
  FLOAT_DMEM * unsorted = row->dataF;
  FLOAT_DMEM * sorted=nullptr;
  
  if (nonZeroFuncts) {
    NN = 0;
    unsorted = (FLOAT_DMEM*)malloc(sizeof(FLOAT_DMEM)*row->nT);
    if (nonZeroFuncts == 2) {
      for (i=0; i<row->nT; i++) {
        if (row->dataF[i] > 0.0) unsorted[NN++] = row->dataF[i];
      }
    } else {
      for (i=0; i<row->nT; i++) {
        if (row->dataF[i] != 0.0) unsorted[NN++] = row->dataF[i];
      }
    }
  }

  if (requireSorted) {
    sorted = (FLOAT_DMEM*)malloc( sizeof(FLOAT_DMEM)*NN );
    // quicksort:
    memcpy( sorted, unsorted, sizeof(FLOAT_DMEM) * NN );
    // TODO: check for float_dmem::: with #if ...
    #if FLOAT_DMEM_NUM == FLOAT_DMEM_FLOAT
    smileUtil_quickSort_float( sorted, NN );
    #else
    smileUtil_quickSort_double( sorted, NN );
    #endif
  }

  // find max and min value, also compute arithmetic mean
  // these 3 values are required by a lot of functionals, so we do it here..
  FLOAT_DMEM *x=unsorted;
  FLOAT_DMEM min=*x,max=*x;
  double mean=*x;
  FLOAT_DMEM *xE = unsorted+NN;
  while (++x<xE) {
    if (*x<min) min=*x;
    if (*x>max) max=*x;
    mean += (double)*x;
  } mean /= (double)NN;
  
  FLOAT_DMEM *curY = y;
  for (i=0; i<nFunctionalsEnabled; i++) {
    if (functObj[i] != nullptr) {
      functObj[i]->setInputPeriod(getInputPeriod());
      int ret;
      ret = functObj[i]->process( unsorted, sorted, min, max, (FLOAT_DMEM)mean, curY, NN, functN[i] );
      if (ret < functN[i]) {
        int j;
        for (j=ret; j<functN[i]; j++) curY[j] = 0.0;
      }
      if (ret>0) ok++;
  	  curY += functN[i];
    }
  }

  if (requireSorted) {
    free(sorted);
  }
  if (nonZeroFuncts) {
    free(unsorted);
  }
  
  return nFunctValues;

/*
  } else if (row->type == DMEM_INT) {
    // TODO....
    SMILE_ERR(1,"type DMEM_INT not yet supported in cFunctionals!");
    return 0;
  } else {
    SMILE_ERR(1,"unknown datatype encountered in cFunctionals doProcess!");
    return 0;
  }
*/

}

/* TODO
int cFunctionals::doProcess(int idxi, cMatrix *row, INT_DMEM*y)
{
  // copy row to matrix... simple memcpy here
  //memcpy(y,row->dataI,row->nT*sizeof(INT_DMEM));
  // return the number of components in y!!
  return nFunctsEnabled;
}
*/

cFunctionals::~cFunctionals()
{
  int i;
  if (functTp != nullptr) {
    for (i=0; i<nFunctTpAlloc; i++)
      if (functTp[i] != nullptr) free(functTp[i]);
    free(functTp);
  }
  if (functI != nullptr) free(functI);
  if (functN != nullptr) free(functN);
  if (functTpI != nullptr) free(functTpI);
  if (functObj != nullptr) {
    for (i=0; i<nFunctTpAlloc; i++)
      if (functObj[i] != nullptr) delete(functObj[i]);
    free(functObj);
  }
}

////  to implement in a cFunctionalXXXX object:
/*
(fetchConfig)
NO myCOnfigure / my Finalise !!
getNoutputValues
getValueName
getRequireSorted
process
*/
