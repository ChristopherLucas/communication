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

LibSVM live classifier/regressor
03/2016: added support for LibLINEAR models

*/

#include <classifiers/libsvmliveSink.hpp>
#include <R.h>
#include <Rdefines.h>
#define MODULE "cLibsvmLiveSink"

#ifdef BUILD_COMPONENT_LibsvmLiveSink

SMILECOMPONENT_STATICS(cLibsvmLiveSink)

//sComponentInfo * cLibsvmLiveSink::registerComponent(cConfigManager *_confman)
SMILECOMPONENT_REGCOMP(cLibsvmLiveSink)
{
  SMILECOMPONENT_REGCOMP_INIT
  scname = COMPONENT_NAME_CLIBSVMLIVESINK;
  sdescription = COMPONENT_DESCRIPTION_CLIBSVMLIVESINK;

  // we inherit cDataSink configType and extend it:
  SMILECOMPONENT_INHERIT_CONFIGTYPE("cDataSink")

  SMILECOMPONENT_IFNOTREGAGAIN_BEGIN
  ct->makeMandatory(ct->setField("model","LibSVM or LibLINEAR model file(s) to load (see isLibLinearModel option for libLinear).", "svm.model", ARRAY_TYPE));
#ifdef BUILD_LIBLINEAR
  ct->setField("isLibLinearModel", "1 = load the model(s) as a liblinear model, not libsvm.", 0);
#endif
  ct->setField("scale","LibSVM scale file(s) to load", (const char *) nullptr, ARRAY_TYPE);
  ct->setField("fselection","Feature selection file(s) to apply (leave empty to use all features). The feature selection files must contain the exact names of the selected features, one feature per line.",(const char*)nullptr,ARRAY_TYPE);
  ct->setField("nIgnoreEndSelection","number of string lines to ignore at the *end* of the feature selection file (only works for string/name lists, not for index lists!)",0);
  ct->setField("classes","Class name lookup file(s), which map the libsvm class indicies to actual class names (leave empty to display libsvm class numbers/indicies) [note: currently only ONE class name lookup file is supported!]",(const char*)nullptr,ARRAY_TYPE);
  ct->setField("predictProbability","1 = predict class probabilities (confidences) (0 = no)",0);
  ct->setField("printParseableResult","1 = print easily parseable classification/regression result to stdout", 0);
  ct->setField("printResult","1 = print classification/regression result to log", 0);
  ct->setField("saveResult","filename of text file the result(s) will be saved to",(const char*)nullptr);
  ct->setField("resultRecp","List of component(s) to send 'classificationResult' messages to (use , to separate multiple recepients), leave blank (nullptr) to not send any messages",(const char *) nullptr);
  ct->setField("resultMessageName","Freely defineable name that is sent with 'classificationResult' message","svm_result");
  ct->setField("forceScale","1 = for the input values, enforce the range specified in the scale file by clipping out-of-range values (after scaling).",1);
  ct->setField("lag","read data <lag> frames behind (should always remain 0 for this component...?)",0,0,0);
  ct->setField("useThread","1 = load the model and do the classification in a background thread, the data frames (inputs) will be stored in a queue and processed sequentially by the background thread (parallel processing is not implemented yet!).",0);
  ct->setField("loadModelBg","1 = if useThread=1 (and only then...) load the libsvm model and scale files in the background thread. openSMILE will start to run the tick loop, but classify incoming frames only after the model has been loaded. Up to then all incoming frames are discarded.",1);
  ct->setField("threadQueSize","max. number of frames to keep in queue (Set to 0 for an infinite number of frames).",0);
  ct->setField("multiModelMode","1 = classify input data with all loaded models *in parallel* (you will have nModels output messages then). 0 = classify with first model by default. Switching of models is possible via a 'svmSinkSetModel' smile message.",0);
  ct->setField("batchMode","1 = similar to multiModelMode=1, classify using all models, however only one result message (containing multiple individual results) will be generated.",0);
  ct->setField("noVerify","1 = *DON'T* verify whether the support vector dimension matches the feature selection list dimension and the scale file dimension.",0);
  ct->setField("modelResultName","Array of custom names sent as 'custData2' pointer with the classificationResult message for corresponding models. The dimensions of this array should match the dimensions of the 'model' array.",(const char *)nullptr,ARRAY_TYPE);
  ct->setField("bgThreadPriority","The thread priority of the background predictor thread (currently only supported on windows), values -15 (idle) to 15 (real-time), while 0 is normal. This should be lower as or equal to the priority of the openSMILE main thread!",0);

  SMILECOMPONENT_IFNOTREGAGAIN_END

    SMILECOMPONENT_MAKEINFO(cLibsvmLiveSink);
}

SMILECOMPONENT_CREATE(cLibsvmLiveSink)

//-----

cLibsvmLiveSink::cLibsvmLiveSink(const char *_name) :
cDataSink(_name),
classesarray(nullptr),modelarray(nullptr), scalearray(nullptr), predictProbability(0),
printResult(1), printParseableResult(0),
nModels(0),currentModel(0),
nClassFiles(0), nScales(0), nFselections(0),
sendResult(0),
resultMessageName(nullptr), resultRecp(nullptr),
threadRunning(0), modelLoaded(0),
models(nullptr), singlePreprocessMultiModel(0),
abortLater(0), nIgnoreEndSelection(0),
classifierThreadBusy(0), resultFile(nullptr)
{
  smileMutexCreate(runningMtx);
  smileMutexCreate(dataMtx);
  smileCondCreate(dataCond);
  dataFrameQue = new lsvmDataFrameQueue();
}

void cLibsvmLiveSink::fetchConfig()
{
  cDataSink::fetchConfig();
  nModels = getArraySize("model");
  noVerify = getInt("noVerify");
  nIgnoreEndSelection = getInt("nIgnoreEndSelection");
  multiModelMode = getInt("multiModelMode");
  loadModelBg = getInt("loadModelBg");
  batchMode = getInt("batchMode");
  forceScale=getInt("forceScale");
  if (nModels > 0) {
    nScales = getArraySize("scale");
    if ((nScales != nModels)&&(nScales > 1))
      SMILE_IERR(1,"size of 'scale' array is different from size of 'model' array!");
    nFselections = getArraySize("fselection");
    if ((nFselections != nModels)&&(nFselections > 1))
      SMILE_IERR(2,"size of 'fselection' array is different from size of 'model' array!");
    nClassFiles = getArraySize("classes");
    if ((nClassFiles != nModels)&&(nClassFiles > 1))
      SMILE_IERR(2,"size of 'classes' array is different from size of 'model' array!");

    models = new svmModelWrapper[nModels]();
    if (nScales < 0) { nScales = 0; }
    if (nFselections < 0) { nFselections = 0; }
    if (nClassFiles < 0) { nClassFiles = 0; }
    int nModelResultNames = getArraySize("modelResultName");
    for (int i=0; i<nModels; i++) {
      models[i].isLibLinearModel = getInt_f(myvprint("isLibLinearModel[%i]", i));
      models[i].modelFile = getStr_f(myvprint("model[%i]",i));
      if (nModelResultNames > 0) {
        if (i<nModelResultNames) models[i].modelResultName = getStr_f(myvprint("modelResultName[%i]",i));
        else {
          SMILE_IERR(2,"less model result names given than there are models!");
        }
      }
      models[i].noVerify = noVerify;
    }
    for (int i=0; i<nScales; i++) {
      models[i].scaleFile = getStr_f(myvprint("scale[%i]",i));
    }
    for (int i=0;i<nClassFiles;i++) {
      models[i].classesFile = getStr_f(myvprint("classes[%i]",i));
    }
    for (int i=0; i<nFselections; i++) {
      models[i].fselectionFile = getStr_f(myvprint("fselection[%i]",i));;
      models[i].nIgnoreEndSelection = nIgnoreEndSelection;
    }
  } else {
    SMILE_IERR(1,"nModels < 1 ! You must specify at least one model to load (see the 'model=' option)!!");
    COMP_ERR("aborting...");
  }
  predictProbability = getInt("predictProbability");
  for (int i=0;i<nModels;i++) {
    models[i].predictProbability = predictProbability;
    if (models[i].modelFile != nullptr) {
      SMILE_IDBG(2,"Model #%i : filename of SVM model to load = '%s'",i,models[i].modelFile);
    }
    if (nScales > 1 || i==0) {
      SMILE_IDBG(2,"Model #%i : filename of scale data to load = '%s'",i,models[i].scaleFile);
    } else { 
      if (nScales > 0) { 
        models[i].setTemplate(models);
        SMILE_IDBG(2,"Model #%i : using common feature scaling for all models = '%s'",i,models[0].scaleFile); 
      }
    }
    if (nFselections > 1 || i==0) { SMILE_IDBG(2,"Model #%i : filename of feature selection file to load = '%s'",i,models[i].fselectionFile); }
    else { 
      if (nFselections > 0) { 
        models[i].setTemplate(models);
        SMILE_IDBG(2,"Model #%i : using common feature selection list for all models = '%s'",i,models[0].fselectionFile); 
      }
    }
    if (nClassFiles > 1 || i==0) { SMILE_IDBG(2,"Model #%i : filename of class mapping to load = '%s'",i,models[i].classesFile); }
    else { 
      if (nClassFiles > 0) { 
        models[i].setTemplate(models);
        SMILE_IDBG(2,"Model #%i : using common class mapping for all models = '%s'",i,models[0].classesFile); 
      }
    }
  }
  if ((multiModelMode == 1) && (nScales <= 1) && (nFselections <= 1) && (nClassFiles <= 1))
    singlePreprocessMultiModel = 1;
  printParseableResult = getInt("printParseableResult");
  printResult = getInt("printResult");
  resultFile = getStr("saveResult");
  if (resultFile != nullptr)
    saveResult = 1;
  else
    saveResult = 0;
  resultRecp = getStr("resultRecp");
  if (resultRecp != nullptr)
    sendResult = 1;
  resultMessageName = getStr("resultMessageName");
  useThread = getInt("useThread");
  threadQueSize = getInt("threadQueSize");
  if (!singlePreprocessMultiModel && multiModelMode)
    threadQueSize *= nModels;
  bgThreadPriority = getInt("bgThreadPriority");
  if (bgThreadPriority < -15)
    bgThreadPriority = -15;
  if (bgThreadPriority > 15)
    bgThreadPriority = 15;
}


#define MAX_LINE_LENGTH 2048

int svmModelWrapper::loadClasses( const char *file, char *** names )
{
  if ((file != nullptr)&&(names!=nullptr)) {
    if (strlen(file)<1) return 0;

    FILE *f = nullptr;
    if (file != nullptr) f = fopen(file,"r");
    if (f== nullptr) {
      SMILE_ERR(2,"error opening class map file '%s' for reading! NOT using a class map!",file);
      return 0;
    }

    char line[MAX_LINE_LENGTH+1];
    int nCls=0;

    while(fgets(line,MAX_LINE_LENGTH,f) != nullptr) {
      if (strlen( line ) > 1) { 
        line[strlen( line ) - 1] = 0;
        const char *cn = strchr(line,':');
        if (cn!=nullptr) {
          nCls++;
          // TODO: use class number instead of cont. index
        }
      }
    }
    fclose(f);

    *names = (char **) calloc(1,sizeof(char*)*nCls);
    nClassNames = nCls;

    f = fopen(file,"r");
    if (f== nullptr) {
      SMILE_ERR(2,"error opening class map file '%s' for reading (2nd pass)! NOT using a class map!",file);
      return 0;
    }

    int cIdx = 0;

    while(fgets(line,MAX_LINE_LENGTH,f) != nullptr) {
      if (strlen( line ) > 1) { 
        line[strlen( line ) - 1] = 0;
        const char *cn = strchr(line,':');
        if (cn!=nullptr) {
          (*names)[cIdx++] = strdup(cn+1);
          //SMILE_MSG(1,"%s",classNames[classnames_run-1]);
          //if (cn+2 !=nullptr) classNames[classnames_run++] = strdup(cn+2);
          //if (cn+3 !=nullptr) classNames[classnames_run++] = strdup(cn+3);

          // TODO: use class number instead of cont. index
        }
      }
    }
    fclose(f);
    //SMILE_IMSG(3,"classNames[%i] = %s",classnames_run-1,classNames[classnames_run-1]);

    return nCls;
  }
  return 0;
}

int svmModelWrapper::loadSelection(const char *selFile, sFselectionData **fselections)
{
  if ((selFile != nullptr)&&(fselections!=nullptr)) {
    if (strlen(selFile)<1) return 0;

    FILE *f = fopen(selFile,"r");
    if (f== nullptr) {
      SMILE_ERR(2,"error opening feature selection file '%s' for reading! NOT using a feature selection!",selFile);
      return 0;
    }

    *fselections = (sFselectionData*)calloc(1,sizeof(sFselectionData)); 

    // read first line to determine filetype:
    char line[MAX_LINE_LENGTH+1];
    unsigned int nStr = 0;
    unsigned int nStr0 = 0;
    {
      char * fgets_res = nullptr;
      fgets_res = fgets( line, 5, f);
    }
    line[3] = 0;
    if (!strncasecmp(line,"str",3)) { // string list
      (*fselections)->fselType = 2;
      SMILE_DBG(5,"reading string list of features");
      {
        int fscanf_res = 0;      
        fscanf_res = fscanf( f, "%u\n", &nStr0);
      }
      nStr = nStr0-nIgnoreEndSelection;
      if (nStr < 1) { COMP_ERR("Error reading feature selection file, nFeatures < 1 (nIgnoreEndSelection = %i ; nStr0 = %i)!",nIgnoreEndSelection,nStr0); }
      (*fselections)->outputSelStr.n = nStr;
      (*fselections)->Nsel=nStr;
      (*fselections)->outputSelStr.names = (char **)calloc(1,sizeof(char *)*nStr);
      unsigned int i=0; 
      line[0] = 0;
      while(fgets(line,MAX_LINE_LENGTH,f) != nullptr) {
        size_t sl = strlen(line);
        if (sl > 1) { 
          if (i<nStr) {
            if (line[sl - 1] == '\n') {
              line[sl - 1] = 0; sl--;
            }
            if (line[sl - 1] == '\r') {
              line[sl - 1] = 0; sl--;
            }
            if (sl > 0) {
              (*fselections)->outputSelStr.names[i++] = strdup(line);
            } else {
              SMILE_WRN(1,"empty line in feature selection file '%s'",selFile);
            }
          } else { 
            if (i>=nStr0) {
              SMILE_ERR(1,"excess line in feature selection file '%s' : '%s' (expected only %i lines with features)",selFile,line,nStr);
            }
          }
        }
      }
      if ((*fselections)->outputSelStr.n > i) {
        SMILE_WRN(1,"less feature names (only %i) in feature selection file '%s' than specified in the header (%i).",i,selFile,(*fselections)->outputSelStr.n);
        (*fselections)->outputSelStr.n = i;
      }
      SMILE_DBG(5,"enabled %i features",i);
      fclose(f);
      return 1;
    } else if (!strncasecmp(line,"idx",3)) { // index list
      (*fselections)->fselType = 1;
      SMILE_DBG(5,"reading index list of features");
      unsigned int idx = 0;
      int i;
      // pass1: parse for max index
      while(fscanf(f,"%u\n",&idx) == 1)
        (*fselections)->outputSelIdx.nFull = MAX((*fselections)->outputSelIdx.nFull, idx);
      SMILE_DBG(5,"max index in selection file was found to be %i",(*fselections)->outputSelIdx.nFull);
      (*fselections)->outputSelIdx.nFull++;
      (*fselections)->outputSelIdx.enabled = (long *)calloc(1,sizeof(long)*(*fselections)->outputSelIdx.nFull);
      rewind( f );
      {
        char * fgets_res = nullptr;
        fgets_res = fgets(line, 5, f); // skip header line;
      }      
      // pass2: parse for max index
      i=0;
      while(fscanf(f,"%u\n",&idx) == 1) {
        (*fselections)->outputSelIdx.enabled[idx] = 1; // enable feature "idx"
        i++;
      }
      (*fselections)->outputSelIdx.nSel = i;
      (*fselections)->Nsel = i;
      SMILE_DBG(5,"enabled %i features",i);
      fclose(f);
      return 1;
    } else { // bogus file...
      fclose( f );
      COMP_ERR("error parsing fselection file '%s'. bogus header! expected 'str' or 'idx' at beginning. found '%s'.",selFile,line);
    }
  }
  return 0;
}

// load model, scale, etc. or copy from template, if template is set and filename is nullptr
int svmModelWrapper::load() 
{
  if (modelFile == nullptr)
    return 0;
  // model file
  if (isLibLinearModel) {
#ifdef BUILD_LIBLINEAR
    modelLinear = liblinear_load_model(modelFile);
    if (modelLinear == nullptr) {
      SMILE_ERR(1, "svmModelWrapper: can't open LibLinear model file '%s'", modelFile);
      return 0;
    }
    nClasses = modelLinear->nr_class;
    // TODO: labels/regression/type

#else
    SMILE_ERR(1, "LibLinear is not supported by this openSMILE build!");
    COMP_ERR("aborting");
#endif
  } else {
    if ((model = svm_load_model(modelFile))==0) {
      SMILE_ERR(1, "svmModelWrapper: can't open LibSVM model file '%s'",modelFile);
      return 0;
    }
    nClasses = svm_get_nr_class(model);
    svmType = svm_get_svm_type(model);
    if (predictProbability) {
      if ((svmType==NU_SVR) || (svmType==EPSILON_SVR)) {
        nClasses = 0;
        SMILE_MSG(3,"LibSVM prob. model (regression) for test data: target value = predicted value + z,\nz: Laplace distribution e^(-|z|/sigma)/(2sigma),sigma=%g",svm_get_svr_probability(model));
      } else {
        labels=(int *) malloc(nClasses*sizeof(int));
        svm_get_labels(model,labels);
        SMILE_MSG(3,"LibSVM %i labels in model '%s':",nClasses,modelFile);
        int j;
        for(j=0;j<nClasses;j++) {
          SMILE_MSG(3,"  Label[%i] : '%d'",j,labels[j]);
        }
      }
    }
  }

  // scale file
  if (scaleFile != nullptr) {
    if ((scale = svm_load_scale(scaleFile))==0) {
      SMILE_ERR(1,"can't open libSVM scale file '%s'",scaleFile);
      return 0;
    }
  } else {
    if (templ != nullptr) {
      alienScale = 1;
      scale = templ->scale;
    } else {
      scale = nullptr;
    }
  }
  // load feature selection(s)
  if (fselectionFile != nullptr) {
    if (!loadSelection(fselectionFile,&fselection)) return 0;
  } else {
    if (templ != nullptr) {
      alienFselection = 1;
      fselection = templ->fselection;
    } else {
      // dummy fsel with nullptr values
      fselection = (sFselectionData*)calloc(1,sizeof(sFselectionData)); 
    }
  } 

  long svSize = 0;
  if (isLibLinearModel) {
#ifdef BUILD_LIBLINEAR
    svSize = modelLinear->nr_feature;
#endif
  } else {
    svSize = svm_get_sv_size(model);
  }
  // verify libsvm/liblinear model dimensions
  int xret = 1;
  if ((fselection != nullptr) && (fselection->Nsel > 0 && fselection->Nsel != svSize)) {
    SMILE_WRN(1,"number of selected features (%i) does not match the data/vector size in the model (%i)!", fselection->Nsel, svSize);
    xret = 0;
  }
  if ((scale != nullptr) && (scale->max_index != svSize)) {
    SMILE_WRN(1,"number of features to scale (%i) does not match the data/vector size in the model (%i)!", scale->max_index, svSize);
    xret = 0;
  }
  if ((xret == 0) && (!noVerify))
    return 0;

  //TODO: check consistency of model:
  // nFsel == nScale == nFeaturesInModel !!
  // class names
  if (classesFile != nullptr) {
    if (nClasses>0) {
      int nCls;
      if (!(nCls = loadClasses(classesFile,&classNames))) return 0;
      if (nCls != nClasses) {
        SMILE_WRN(1,"number of classes in classesFile (%i) doesn't match number of classes in model (%i) for model '%s' (classesfile = '%s')",nCls,nClasses,modelFile,classesFile);
      }
    } else {
      SMILE_WRN(2,"not loading given class mapping file for regression SVR model (there are no classes...)!");
    }
  } else {
    if ((nClasses > 0)&&(templ!=nullptr)) {
      alienClassnames = 1;
      classNames = templ->classNames;
    }
  }
  return 1;
}



SMILE_THREAD_RETVAL libsvmliveThreadRunner(void *_data)
{
  if ((_data != nullptr)) {
    ((cLibsvmLiveSink *)_data)->classifierThread();
  }
  SMILE_THREAD_RET;
}

int cLibsvmLiveSink::myFinaliseInstance()
{
  int ret = cDataSink::myFinaliseInstance();
  if (ret==0) return 0;

  if (useThread) {
    if (!loadModelBg) {
      loadClassifier();
      modelLoaded = 1;
    }
    // start the classifier background thread:
    // this thread first loads the model and then processes data frames
    smileMutexCreate(dataMtx);
    smileMutexCreate(runningMtx);
    smileCondCreate(dataCond);
    threadRunning = 1;
    if (!(int)smileThreadCreate(bgThread, libsvmliveThreadRunner, (void *)this)) {
      SMILE_ERR(1,"error creating libsvm background thread, multi-threading disabled!!");
      threadRunning = 0;
    } else {
      /* TODO: set thread priority ... */
#if defined(WIN32)
      SMILE_IMSG(3,"current bgThread priority = %i",GetThreadPriority(bgThread));
      SetThreadPriority(bgThread, bgThreadPriority);
      SMILE_IMSG(3,"bgThread priority now set to %i",GetThreadPriority(bgThread));
#endif
    }
  } else {
    loadClassifier();
    modelLoaded = 1;
  }
  return ret;
}

void cLibsvmLiveSink::processResult(lsvmDataFrame *f, int modelIdx, int mmm /* multiModelMode */)
{
  //long long tick, long frameIdx, double time, float res, double *probEstim, char **classNames, int nClasses, double dur, int isFinal

  int clIdx = -1;
  if (printParseableResult || printResult || sendResult || saveResult) {
    /*if (models[modelIdx].labels!=nullptr) {
      int r = (int)(f->res);
      for (int i=0; i<models[modelIdx].nClasses; i++) {
        if (models[modelIdx].labels[i] == r) { clIdx = i;  }
      }
      if (clIdx == -1) {
        SMILE_IERR(1,"svm error, result label (%f) not found in model->labels array!",f->res);
      }
    } else {
      clIdx = (int)(f->res);
    }*/
    // ??? ok ???
    clIdx = (int)(f->res);
  }

  if (printParseableResult || printResult || saveResult) {
    FILE * fl = nullptr;
    if (saveResult) {
      fl = fopen(resultFile,"a");
      if (fl==nullptr) {
        SMILE_IERR(1,"cannot open result output file '%s' for writing (appending)! Disabling saving of classification result to file. No more errors will be shown.");
        saveResult = 0;
      }
    }

    if ((models[modelIdx].nClasses > 0)&&(models[modelIdx].classNames != nullptr)) {
      if (clIdx >= models[modelIdx].nClasses) clIdx = models[modelIdx].nClasses;
      else if (clIdx < 0) clIdx = 0;
      if (printResult) {
        SMILE_PRINT("\n LibSVM  '%s' (#%i = %s) result [final=%i] (@ time: %f, vIdx: %ld) :  ~~> %s <~~",getInstName(),modelIdx,models[modelIdx].modelResultName,f->isFinal,f->time, f->frameIdx, models[modelIdx].classNames[clIdx]);
      }
      if (printParseableResult) {
        printf("SMILE-RESULT::ORIGIN=libsvm::TYPE=classification::COMPONENT=%s::VIDX=%ld::NAME=%s::CATEGORY_IDX=%i::CATEGORY=%s", getInstName(), f->frameIdx, models[modelIdx].modelResultName, (int)(clIdx), models[modelIdx].classNames[clIdx]);
      }
      if (saveResult) {
        fprintf(fl," LibSVM  '%s' (#%i = %s) result [final=%i] (@ time: %f, vIdx: %ld) :  ~~> %s <~~\n",getInstName(),modelIdx,models[modelIdx].modelResultName,f->isFinal,f->time, f->frameIdx, models[modelIdx].classNames[clIdx]);
      }
    } else {
      if (printParseableResult) {
        printf("SMILE-RESULT::ORIGIN=libsvm::TYPE=regression::COMPONENT=%s::VIDX=%ld::NAME=%s::VALUE=%e", getInstName(), f->frameIdx, models[modelIdx].modelResultName, f->res);
      }
      if (printResult) {
        SMILE_PRINT("\n LibSVM  '%s' (#%i = %s) result [final=%i] (@ time: %f, vIdx: %ld) :  ~~> %.2f <~~",getInstName(),modelIdx,models[modelIdx].modelResultName,f->isFinal,f->time, f->frameIdx, f->res);
      }
      if (saveResult) {
        fprintf(fl," LibSVM  '%s' (#%i = %s) result [final=%i] (@ time: %f, vIdx: %ld) :  ~~> %.2f <~~\n",getInstName(),modelIdx,models[modelIdx].modelResultName,f->isFinal,f->time, f->frameIdx, f->res);
      }
    }
    if (f->probEstim != nullptr) {
      int i;
      if ((models[modelIdx].svmType==C_SVC || models[modelIdx].svmType==NU_SVC)) {
        for (i=0; i<models[modelIdx].nClasses; i++) {
          int idx = i;
          if (models[modelIdx].labels!=nullptr) idx = models[modelIdx].labels[i];
          if ((models[modelIdx].nClasses > 0)&&(models[modelIdx].classNames != nullptr)) {
            if (idx >= models[modelIdx].nClasses) idx=models[modelIdx].nClasses-1;
            if (idx < 0) idx = 0;
            if (printParseableResult) {
              printf("::PROB=%i;%s:%f", idx, models[modelIdx].classNames[i], f->probEstim[idx]);
            }
            if (printResult) {
              SMILE_PRINT("     prob. class '%s': \t %f",models[modelIdx].classNames[i],f->probEstim[idx]);
            }
            if (saveResult) {
              fprintf(fl,"     prob. class '%s': \t %f",models[modelIdx].classNames[i],f->probEstim[idx]);
            }

          } else {
            if (printParseableResult) {
              printf("::PROB=%i;:%f", idx, f->probEstim[i]);
            }
            if (printResult) {
              SMILE_PRINT("     prob. class %i : \t %f",idx,f->probEstim[i]);
            }
            if (saveResult) {
              fprintf(fl,"     prob. class %i : \t %f",idx,f->probEstim[i]);
            }

          }
        }
      } else {
        if (printParseableResult) {
          printf("::CONFIDENCE=%f", f->svr_confidence);
        }
        if (printResult) {
          SMILE_PRINT("     prob. of result : \t %f",f->svr_confidence);
        }
        if (saveResult) {
          fprintf(fl,"     prob. of result : \t %f",f->svr_confidence);
        }
      }
    }
    if (printParseableResult) {
      printf("\n");
    }
    if (fl != nullptr) fclose(fl);
  }

  // send result as componentMessage 
  if (sendResult && (!batchMode)) {
    cComponentMessage msg("classificationResult", resultMessageName);
	// msgtext contains classification result
    if ((models[modelIdx].nClasses > 0)&&(models[modelIdx].classNames != nullptr)) {
      if (models[modelIdx].labels!=nullptr) {
        if ((int)(f->res) >= models[modelIdx].nClasses) f->res = (float)(models[modelIdx].nClasses-1);
        if (f->res < 0.0) f->res = 0.0;
        f->res = (float)models[modelIdx].labels[(int)(f->res)];
      }
      if ((int)(f->res) >= models[modelIdx].nClasses) f->res = (float)models[modelIdx].nClasses;
      if (f->res < 0.0) f->res = 0.0;
      strncpy(msg.msgtext, models[modelIdx].classNames[(int)clIdx], CMSG_textLen);
    } else {
	  strncpy(msg.msgtext, "_NUMERIC_", 9);
	}
    msg.floatData[0] = f->res;
    // result confidence (float[1])
    if (f->probEstim != nullptr) {
      if ((models[modelIdx].svmType==C_SVC || models[modelIdx].svmType==NU_SVC)) {
        int idx=clIdx;
        if (models[modelIdx].labels!=nullptr) idx = models[modelIdx].labels[clIdx];
        msg.floatData[1] = f->probEstim[idx];
      } else {
        msg.floatData[1] = f->probEstim[0];
      }
    } else {
      msg.floatData[1] = 0.0;
    }
    msg.floatData[2] = msg.floatData[1];  // winning class probability
    msg.intData[0]   = models[modelIdx].nClasses;
    msg.intData[1]   = f->isFinal;
    msg.intData[2]   = modelIdx;
    msg.intData[3]   = mmm;
    msg.intData[5]   = f->ID;
    if (models[modelIdx].modelResultName != nullptr) {
      strncpy(msg.msgname, models[modelIdx].modelResultName, CMSG_typenameLen);
    }
    msg.custData     = f->probEstim;
    msg.custData2    = (void *) ( models[modelIdx].modelResultName ); // dangerous conversion....! This is left only for backward compatibility. Use msgtext in new designs!!
    msg.userTime1    = f->time;
    msg.userTime2    = f->time + f->dur;

    sendComponentMessage( resultRecp, &msg );
    SMILE_IDBG(3,"sending 'classificationResult' message to '%s'",resultRecp);
    // warning: the probEstim pointer is not valid anymore, after sendComponentMessage has exited..!
  } else if (batchMode) {
    int i;
    // add result to cache..

    resCache.res[resCache.nFilled] = f->res;
    for (i=0; i<resCache.nTargets; i++) {
      if ((i<f->nClasses)&&(f->probEstim != nullptr)) resCache.prob[resCache.nFilled*resCache.nTargets+i] = f->probEstim[i];
      else resCache.prob[resCache.nFilled*resCache.nTargets+i] = -1.0;
    }
    resCache.resnameA[resCache.nFilled] = models[modelIdx].modelResultName;
    resCache.nFilled++;

    if (f->isLast) { // send the message...
      cComponentMessage msg("classificationResults", resultMessageName);
      msg.userTime1    = f->time;
      msg.userTime2    = f->time + f->dur;
      msg.intData[1]   = f->isFinal;
      msg.custData     = (void *)&resCache;

      sendComponentMessage( resultRecp, &msg );
      for (i=0; i<resCache.nFilled; i++) {
        //if (resCache.resnameA[i] != nullptr) free(resCache.resnameA[i]);
        resCache.resnameA[i] = nullptr;
      }
      resCache.nFilled = 0;
      SMILE_IDBG(3,"sending 'classificationResults' message to '%s'",resultRecp);
    }

  }
}

/*
multi classification result message:

id[0] : nClasses (0=regression)
id[1] : isFinal
id[2] : nModels
id[3] : 1 (to indicate multimodelmode)
userTime1 : segment start time
userTime2 : segment end time

custData : pointer to multi result struct:
{
  int nResults
  double *res;
  const char ** resnameA;
  const char ** resnameB;
}

*/


int cLibsvmLiveSink::buildEnabledSelFromNames(long N, const FrameMetaInfo *fmeta, sFselectionData *fselection)
{
  int n,i;
  SMILE_IDBG(3,"computing mapping from feature names to feature indicies");
  fselection->outputSelIdx.nFull = N;
  fselection->outputSelIdx.nSel = fselection->outputSelStr.n;
  fselection->outputSelIdx.enabled = (long *)calloc(1,sizeof(long)*fselection->outputSelIdx.nFull);
  fselection->outputSelIdx.map = (long *)calloc(1,sizeof(long)*fselection->outputSelIdx.nSel);
  int nEnab=0;
  int *isFound = (int*)calloc(1,sizeof(int)*fselection->outputSelStr.n);
  for (n=0; n<N; n++) {
    //  int found=0;
    //  SMILE_IDBG(4,"selstr %i of %i",i,outputSelStr.n);
    const char * tmpname = reader_->getElementName(n);
    
    for (i=0; i<fselection->outputSelStr.n; i++) {
      if (!isFound[i]) {
        
        if (!strcmp(tmpname,fselection->outputSelStr.names[i])) {
          fselection->outputSelIdx.enabled[n] = 1;
          fselection->outputSelIdx.map[i] = n;
          nEnab++; isFound[i]=1;
          break;
        }
      }
    }
    if (nEnab >= fselection->outputSelStr.n) break;
    /*
    if (found==0) {
    SMILE_IERR(1,"element '%s' requested in feature selection list, but it was not found in the input!",outputSelStr.names[i]);
    }*/
  }

  if (nEnab < fselection->outputSelStr.n) {
    SMILE_IERR(1,"%i features which were requested in feature selection file were not found in current input data! please check openSMILE config! These features are:",fselection->outputSelStr.n-nEnab);
    for (i=0; i<fselection->outputSelStr.n; i++) {
      if (isFound[i]==0) {
        SMILE_IERR(1,"  element '%s' ",fselection->outputSelStr.names[i]);
      }
    }
    abortLater=1;
    //COMP_ERR("Your system will not run stable, since the models are incompatible with the currently extracted features. Thus, we are aborting here!");
  } else {
    SMILE_IDBG(3,"mapping computed successfully");
  }
  free(isFound);
  return 1;
}

int cLibsvmLiveSink::loadClassifier()
{
  // load model
  SMILE_IMSG(2,"loading LibSVM model(s) ..."); 
  for (int i=0;i<nModels;i++) {
    if (!models[i].load()) {
      COMP_ERR("failed loading model %i (file: '%s')",i,models[i].modelFile);
    }
  }

  int ncMax = models[0].nClasses;
  for (int i=1;i<nModels;i++) {
    if (models[i].nClasses > ncMax) {
      ncMax = models[i].nClasses;
    }
  }

  // allocate result storage buffer
  if (batchMode && nModels > 0) {
    resCache.nResults = nModels;
    resCache.nTargets = ncMax;
    resCache.alloc();
  }

  // load feature selection(s)
  long N = reader_->getLevelN();
  const FrameMetaInfo *fmeta = reader_->getFrameMetaInfo();
  
  for(int i=0;i<nFselections;i++) {
    SMILE_IDBG(2,"building feature selection map for model %i",i);
    if (models[i].fselection->fselType) {
      if ((models[i].fselection->outputSelIdx.enabled == nullptr)&&(models[i].fselection->outputSelStr.names != nullptr)) {
        buildEnabledSelFromNames(N, fmeta, models[i].fselection);
      } 
    }
  }

  SMILE_IMSG(2,"Models loaded. Ready to classify!"); 
  return 1;
}

// classify data frame with a single model
void cLibsvmLiveSink::processDigestFrame(lsvmDataFrame * f, int modelIdx)
{
  if (models[modelIdx].isLibLinearModel) {
#ifdef BUILD_LIBLINEAR
    // TODO:
    //double predict_values(const struct liblinear_model *model_, const struct svm_node *x, double* dec_values);
    //double predict(const struct liblinear_model *model_, const struct svm_node *x);
    //double predict_probability(const struct liblinear_model *model_, const struct svm_node *x, double* prob_estimates);
    if ((models[modelIdx].predictProbability) && (models[modelIdx].svmType==C_SVC || models[modelIdx].svmType==NU_SVC)) {
      f->res = liblinear_predict_probability(models[modelIdx].modelLinear, f->x, f->probEstim);
      processResult(f, modelIdx, multiModelMode);
    } else {
      f->res = liblinear_predict(models[modelIdx].modelLinear, f->x);
      //f->svr_confidence = svm_get_svr_probability(models[modelIdx].model);
      processResult(f, modelIdx, multiModelMode);
    }
#else
    SMILE_IERR(1, "LibLINEAR not supported by this build version. Ignoring frame.");
#endif
  } else {
    if ( (models[modelIdx].predictProbability) && (models[modelIdx].svmType==C_SVC || models[modelIdx].svmType==NU_SVC) ) {
      f->res = svm_predict_probability(models[modelIdx].model, f->x, f->probEstim);
      processResult(f, modelIdx, multiModelMode);
    } else {
      f->res = svm_predict(models[modelIdx].model, f->x);
      f->svr_confidence = svm_get_svr_probability(models[modelIdx].model);
      processResult(f, modelIdx, multiModelMode);
    }
  }
}

// classify data frame with a single model or all models 
void cLibsvmLiveSink::digestFrame(lsvmDataFrame * f, int modelIdx)
{
  if (modelIdx == -1) { // all models
    int i;
    for (i=0; i<nModels; i++) {
      if (i<nModels-1) { f->isFinal = 0; }
      else { f->isFinal = 1; }
      processDigestFrame(f,i);
    }
  } else { // single model
    processDigestFrame(f,modelIdx);
  }
  delete f;
}

void cLibsvmLiveSink::classifierThread()
{
  //// this thread runs in a loop and waits for data to classify

  // when a new message arrives a thread condition variable is signalled
  smileMutexLock(runningMtx);
  int _running = threadRunning;
  smileMutexUnlock(runningMtx);

  // load the model in the background...
  if (!modelLoaded) loadClassifier(); // note: we don't need to lock the mutex right here, because the variable is set before the thread is called and only read from at the moment

  smileMutexLock(dataMtx);
  modelLoaded = 1;  

  //smileMutexLock(dataMtx);
  while (_running) {
    if (!dataFrameQue->empty()) {
      classifierThreadBusy = 1;

      lsvmDataFrame *f = dataFrameQue->front();
      dataFrameQue->pop();
      smileMutexUnlock(dataMtx);

      // classify frame...
      if (f != nullptr) {
        if (f->modelchoice >= -1 && f->modelchoice < nModels) {
          digestFrame(f, f->modelchoice);
        } else {
          SMILE_IERR(1,"input frame dropped due to invalid model selection (out of range) [%i]  (valid: -1 - %i)",f->modelchoice,nModels-1);
        }
      }

    } else {
      smileMutexUnlock(dataMtx);
    }
    
    smileMutexLock(runningMtx);
    _running = threadRunning;
    smileMutexUnlock(runningMtx);

    smileMutexLock(dataMtx);
    if ((_running)&&(dataFrameQue->empty())) { 
      classifierThreadBusy = 0;
      smileCondWaitWMtx(dataCond,dataMtx);
    }
  }
  classifierThreadBusy = 0;
  smileMutexUnlock(dataMtx);
}

int cLibsvmLiveSink::queFrameToClassify(lsvmDataFrame *fr)
{
  // add data in 'fr' to queue
  smileMutexLock(dataMtx);
  if ((threadQueSize > 0)&&(dataFrameQue->size() == threadQueSize)) {
    lsvmDataFrame *f = dataFrameQue->front();
    if (f != nullptr) delete f; 
    dataFrameQue->pop();
  }
  classifierThreadBusy = 1;
  dataFrameQue->push(fr);
  smileMutexUnlock(dataMtx);

  // signal thread condition
  smileCondSignal(dataCond);

  return 1;
}

struct svm_node * cLibsvmLiveSink::preprocessFrame(int modelIdx, cVector *vec)
{
  struct svm_node *x = nullptr;
  long Nft = models[modelIdx].fselection->Nsel;
  if (Nft <= 0) Nft = vec->N;

  int i = 0;
  int j = 0; 
  //double v;

  // need one more for index = -1
  x = (struct svm_node *) malloc( (Nft + 1) * sizeof(struct svm_node));

  // select
  if ((models[modelIdx].fselection->outputSelIdx.enabled == nullptr)||(models[modelIdx].fselection->Nsel<=0)) {
    // copy all features
    for (i=0; i<vec->N; i++) {
      x[i].index = i+1; // FIXME!!! +1 is ok??? (no!?)
      x[i].value = (FLOAT_DMEM)(vec->dataF[i]);
    }
    x[i].index = -1;
    x[i].value = 0.0;
  } else {
    // select features
    for (i=0; i<models[modelIdx].fselection->Nsel; i++) {
      x[i].index = i+1;
      if ( (models[modelIdx].fselection->outputSelIdx.map[i] < vec->N)&&(models[modelIdx].fselection->outputSelIdx.map[i] >= 0) ) 
        x[i].value = (FLOAT_DMEM)(vec->dataF[models[modelIdx].fselection->outputSelIdx.map[i]]);
      else 
        x[i].value = 0.0;
    }
    x[i].index = -1;
    x[i].value = 0.0;
  }

  // scale  
  if (models[modelIdx].scale != nullptr) {
    svm_apply_scale(models[modelIdx].scale, x);

    if (forceScale) {
      // clip values??
      i=0;
      while(1) {
        if (x[i].index == -1) { break; }
        else {
          if (x[i].value > models[modelIdx].scale->upper) { x[i].value = models[modelIdx].scale->upper; }
          else if (x[i].value < models[modelIdx].scale->lower) { x[i].value = models[modelIdx].scale->lower; }
        }
        i++;
      }
    }
  }


  return x;
  /*
  if (fselections[modelchoice].fselType) {
    if ((fselections[modelchoice].outputSelIdx.enabled == nullptr)&&(fselections[modelchoice].outputSelStr.names != nullptr)) {
      //buildEnabledSelFromNames(vec->N, vec->fmeta);
      SMILE_IWRN(1,"feature selection map was not yet built! this is a bug!");
    } else if (fselections[modelchoice].outputSelIdx.enabled == nullptr) Nft = vec->N;
  }
  */
  // TODO: fselection by names... 
  // TODO: compute Nsel in loadSelection
}


int cLibsvmLiveSink::myTick(long long t)
{
  if (abortLater) {
    COMP_ERR("Your system will not run stable, since the models are incompatible with the currently extracted features. Thus, we are aborting here!");
  }
  // check if model has been loaded
  if (useThread && loadModelBg) {
    smileMutexLock(dataMtx);
    int _modelLoaded = modelLoaded;
    smileMutexUnlock(dataMtx);
    if (!_modelLoaded) {
      cVector *vec= reader_->getNextFrame();
      if (vec != nullptr) { SMILE_IWRN(4,"SVM model has not been loaded yet, discarding current input"); }
      return 0;
    }
  }

  // check for valid models
  if (multiModelMode) {
    int i;
    for (i=0; i<nModels; i++) {
      if (models[i].model == nullptr) {
        SMILE_IERR(1,"model %i == nullptr, not classifying input...!",i);
        return 0;
      }
    }
  } else {
    if (models[currentModel].model == nullptr) {
      SMILE_IERR(1,"model %i == nullptr, not classifying input...!",currentModel);
      return 0;
    }
  }


  cVector *vec= reader_->getNextFrame();
  if (vec == nullptr) {
    /* here we must check if the que contains data and the background thread is processing 
       if the background thread is classifying we must return 1 here to prevent the main loop
       from terminating, which will lead to invalid memory access from the classifier thread
    */
    smileMutexLock(dataMtx);
    if (classifierThreadBusy) {
      smileMutexUnlock(dataMtx);
      return 1;
    } else {
      smileMutexUnlock(dataMtx);
      return 0;
    }
  } 
  SMILE_DBG(4,"tick # %i, classifiy value vector using LibSVM:",t);
  SMILE_IMSG(4,"got a vector to process...");

  /* 
  A) multiModelMode:
    a) all models independent
    b) multi-models, common scaling, selection, names, etc.
  B ) single model switching
  */

  long vi = vec->tmeta->vIdx;
  double tm = vec->tmeta->smileTime;
  double dur = vec->tmeta->lengthSec;
  int isFinal = vec->tmeta->metadata.iData[0];
  int customID = vec->tmeta->metadata.ID;

  if (multiModelMode) {

    struct svm_node * x = nullptr;
    lsvmDataFrame *f = nullptr;
    if (singlePreprocessMultiModel) {
      long Nft = models[0].fselection->Nsel;
      if (Nft <= 0) Nft = vec->N;

      x = preprocessFrame(0, vec);
      // the frame is created here, and freed by the thread when it is pop'ed from the queue
      f = new lsvmDataFrame(x,(Nft + 1) * sizeof(struct svm_node), -1, t,vi,tm, 0.0, models[currentModel].predictProbability, models[currentModel].nClasses, dur, isFinal, customID);
      f->setLast();
      if ((useThread)&&(threadRunning)) {
        queFrameToClassify(f);
      } else { // run the old 'inline' code:
        digestFrame(f, currentModel);
      }

    } else {

      int i;
      // NOTE: the threadQueSize must be larger in this case...(see fetchConfig())
      for (i=0; i<nModels; i++) {
        long Nft = models[i].fselection->Nsel;
        if (Nft <= 0) Nft = vec->N;

        x = preprocessFrame(i, vec);
        // the frame is created here, and freed by the thread when it is pop'ed from the queue
        f = new lsvmDataFrame(x,(Nft + 1) * sizeof(struct svm_node), i, t,vi,tm, 0.0, models[currentModel].predictProbability, models[currentModel].nClasses, dur, isFinal, customID);
        if (i==nModels-1) { f->setLast(); }
        if ((useThread)&&(threadRunning)) {
          queFrameToClassify(f);
        } else { // run the old 'inline' code:
          digestFrame(f, i);
        }
      }

    }

    if (x != nullptr)
      free(x);

  } else {
    // verify currentModel
    if (currentModel < 0) currentModel = 0;
    if (currentModel >= nModels) currentModel = nModels-1;

    long Nft = models[currentModel].fselection->Nsel;
    if (Nft <= 0) Nft = vec->N;

    struct svm_node * x = preprocessFrame(currentModel, vec);

    // the frame is created here, and freed by the thread when it is pop'ed from the queue
    lsvmDataFrame *f = new lsvmDataFrame(x,(Nft + 1) * sizeof(struct svm_node),currentModel ,t,vi,tm, 0.0, models[currentModel].predictProbability, models[currentModel].nClasses, dur, isFinal, customID);
    f->setLast();

    if ((useThread)&&(threadRunning)) {
      queFrameToClassify(f);
    } else { // run the old 'inline' code:
      digestFrame(f, currentModel);
    }

    free(x);

  }

   // tick success
  return 1;
}

int cLibsvmLiveSink::processComponentMessage( cComponentMessage *_msg )
{
  if (isMessageType(_msg,"KeyClassforChord"))  { // <- special message for ADMIRE
    currentModel = (_msg->intData[0]);
  } else if (isMessageType(_msg,"svmSinkSetModel"))  { // choose current SVM model by index // TODO: choose by name
    currentModel = (_msg->intData[0]);
  }

  return 0;
}

cLibsvmLiveSink::~cLibsvmLiveSink()
{
  smileMutexLock(runningMtx);
  threadRunning = 0;
  smileMutexUnlock(runningMtx);

  if (useThread) {
    SMILE_IMSG(3,"waiting for classifier thread to terminate ...");
  }
  //    smileMutexUnlock(dataMtx);
  // signal thread condition, to allow thread to wake and exit
  smileCondSignal(dataCond);

  if (useThread) {
    smileThreadJoin(bgThread);
    SMILE_IMSG(3,"classifier thread terminated");  
  }

  if (models != nullptr)
    delete[] models;

  smileMutexDestroy(runningMtx);
  smileMutexDestroy(dataMtx);
  smileCondDestroy(dataCond);
  if (dataFrameQue != nullptr)
    delete dataFrameQue;
}


/**************************************************************************/
/*********              LibSVM   addon:   scale functions  ****************/
/**************************************************************************/
//
// See src/classifiers/libsvm/COPYRIGHT for license and copying information !!
// 
// Modified by Florian Eyben
//

#include <float.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#ifndef WIN32
#define max(x,y) (((x)>(y))?(x):(y))
#define min(x,y) (((x)<(y))?(x):(y))
#endif

/*
struct svm_scale {
int max_index;
double lower,upper;
double y_lower,y_upper;
double y_min,y_max;
double *feature_max;
double *feature_min;
};
*/
char *line = nullptr;
int max_line_len = 1024;

char* readline(FILE *input)
{
  int len;

  if (line == nullptr) line = (char *) calloc(1, max_line_len*sizeof(char));

  if(fgets(line,max_line_len,input) == nullptr)
    return nullptr;

  while(strrchr(line,'\n') == nullptr)
  {
    max_line_len *= 2;
    line = (char *) realloc(line, max_line_len);
    len = (int) strlen(line);
    if(fgets(line+len,max_line_len-len,input) == nullptr)
      break;
  }
  return line;
}

struct svm_scale * svm_load_scale(const char* restore_filename)
{

  if (restore_filename==nullptr) { return nullptr; }

  int idx, c;
  FILE *fp_restore = nullptr;
  struct svm_scale *ret = nullptr;
  double fmin, fmax;
  double y_max = -DBL_MAX;
  double y_min = DBL_MAX;

  fp_restore = fopen(restore_filename,"r");
  if(fp_restore==nullptr)
  {
    Rprintf("can't open scale file %s\n", restore_filename);
    return nullptr;
  }

  c = fgetc(fp_restore);
  if(c == 'y')
  {
    readline(fp_restore);
    readline(fp_restore);
    readline(fp_restore);
  }
  readline(fp_restore);
  readline(fp_restore);

  ret = (struct svm_scale *) calloc(1, sizeof(struct svm_scale) );
  if (ret == nullptr)
  {
    Rprintf("Error allocating memory!\n");
    return nullptr;
  }
  
  while(fscanf(fp_restore,"%d %*f %*f\r\n",&idx) == 1)
    ret->max_index = (idx > ret->max_index) ? (idx) : (ret->max_index);
   
  rewind(fp_restore);


  ret->feature_max = (double *)calloc(1,(ret->max_index+1)* sizeof(double));
  ret->feature_min = (double *)calloc(1,(ret->max_index+1)* sizeof(double));
  ret->feature_min[0] = 0.0;
  ret->feature_max[0] = 0.0;

  if((c = fgetc(fp_restore)) == 'y')
  {
    int fscanf_res = 0;
    fscanf_res = fscanf(fp_restore, "%lf %lf\n", &(ret->y_lower), &(ret->y_upper));
    fscanf_res = fscanf(fp_restore, "%lf %lf\n", &(ret->y_min), &(ret->y_max));
    ret->y_scaling = 1;
  }
  else {
    ungetc(c, fp_restore);
    ret->y_scaling = 0;
  }

  ret->lower = -1.0;
  ret->upper = 1.0;

  if (fgetc(fp_restore) == 'x') 
  {
    {
      int fscanf_res = 0;    
      fscanf_res = fscanf(fp_restore, "%lf %lf\n", &(ret->lower), &(ret->upper));
    }
    while(fscanf(fp_restore,"%d %lf %lf\n",&idx,&fmin,&fmax)==3)
    {
      if(idx<=ret->max_index)
      {
        ret->feature_min[idx] = fmin;
        ret->feature_max[idx] = fmax;
      }
    }
  }
  fclose(fp_restore);

  return ret;
}

void svm_destroy_scale(struct svm_scale *scale)
{
  if (scale) {
    if (scale->feature_min) free(scale->feature_min);
    if (scale->feature_max) free(scale->feature_max);
    free(scale);
  }
}

void svm_apply_scale(struct svm_scale *scale, struct svm_node * x)
{
  int i=0;
  if (scale == nullptr) return;
  if (x == nullptr) return;

  while(1)
  {
    if (x[i].index == -1) break;

    if (x[i].index <= scale->max_index)
    {
      int index = x[i].index;
      double value = x[i].value;

      /* skip single-valued attribute */
      if(scale->feature_max[index] == scale->feature_min[index]) { 
        x[i].value = 0;  /* ??? */
        i++; 
        continue; 
      }

      if(value == scale->feature_min[index])
        value = scale->lower;
      else if(value == scale->feature_max[index])
        value = scale->upper;
      else
        value = scale->lower + (scale->upper - scale->lower) *
        (value- scale->feature_min[index])/
        (scale->feature_max[index] - scale->feature_min[index]);

      x[i].value = value;

    }
    i++;
  }

}

#endif  // BUILD_COMPONENT_LibsvmLiveSink


/**************************************************************************/

