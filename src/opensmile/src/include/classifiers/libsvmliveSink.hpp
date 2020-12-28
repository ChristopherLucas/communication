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

inherit this class, if you want custom handling of classifier output values..

*/


#ifndef __CLIBSVMLIVESINK_HPP
#define __CLIBSVMLIVESINK_HPP

#include <core/smileCommon.hpp>

//#define BUILD_LIBSVM  // only temporary
//#define BUILD_LIBLINEAR

#ifdef BUILD_LIBSVM
#define BUILD_COMPONENT_LibsvmLiveSink

#include <core/smileComponent.hpp>
#include <core/dataSink.hpp>
#include <classifiers/libsvm/svm.h>
#ifdef BUILD_LIBLINEAR
#include <classifiers/liblinear/linear.h>
#endif

#define COMPONENT_DESCRIPTION_CLIBSVMLIVESINK "This component classifies data from dataMemory 'on-the-fly' using the LibSVM or LibLINEAR library. Loading of ASCII and binary LibSVM models is supported, as well as application of LibSVM scale files and openSMILE feature selection lists."
#define COMPONENT_NAME_CLIBSVMLIVESINK "cLibsvmLiveSink"

class  sClassifierResults {
public:
  sClassifierResults() : nFilled(0), nResults(0), res(nullptr), prob(nullptr), resnameA(nullptr), resnameB(nullptr)   {}

  ~sClassifierResults() 
  {
    if (res != nullptr) free(res);
    if (prob != nullptr) free(prob);
    //int i;
    if (resnameA != nullptr) {
      //for (i=0; i<nResults; i++) if (resnameA[i] != nullptr) free(resnameA[i]);
      free(resnameA);
    }
    if (resnameB != nullptr) {
      //for (i=0; i<nResults; i++) if (resnameB[i] != nullptr) free(resnameB[i]);
      free(resnameB);
    }

  }

  void alloc() {
    if (nResults > 0) {
      res = (double *)calloc(1,sizeof(double)*nResults);
      prob = (double *)calloc(1,sizeof(double)*nResults*nTargets);
      resnameA = (const char **)calloc(1,sizeof(const char *)*nResults);
      resnameB = (const char **)calloc(1,sizeof(const char *)*nResults);
    }
  }

  int nFilled;
  int nResults; // N
  int nTargets; // C = number of classes or regression outputs
  double *res;  // dim: 1xN
  double *prob; // dim: CxN
  const char ** resnameA; // custom name1 of result
  const char ** resnameB; // custom name2 of result (can be nullptr)
} ;

/**************************************************************************/
/*********              LibSVM   addon:   scale functions  ****************/
/**************************************************************************/

struct svm_scale {
  int max_index;
  int y_scaling;
  double lower,upper;
  double y_lower,y_upper;
  double y_min,y_max;
  double *feature_max;
  double *feature_min;
};
struct svm_scale * svm_load_scale(const char* restore_filename);
void svm_destroy_scale(struct svm_scale *scale);
void svm_apply_scale(struct svm_scale *scale, struct svm_node * x);

/**************************************************************************/

typedef struct{
  long n;
  char **names;
} sOutputSelectionStr;  // list of names of selected features (more flexible..)
typedef sOutputSelectionStr *pOutputSelectionStr;

typedef struct{
  long nFull;   // n here is the full length of the "unselected" feature vector
  long nSel;
  long *map;
  long *enabled;  // flag : 0 or 1  when feature is disabled/enabled respectively
} sOutputSelectionIdx;  
typedef sOutputSelectionIdx *pOutputSelectionIdx;

typedef struct{
  const char *fselection;
  int fselType; long Nsel;
  sOutputSelectionStr outputSelStr;
  sOutputSelectionIdx outputSelIdx;
} sFselectionData;

#undef class


class  lsvmDataFrame
{ public:

  int isLast; // indicates the last frame for batchMode (a result message will be sent)
  long dataSize; // size of input data struct allocated memory in bytes
  svm_node *x; // the input data
  int modelchoice;
  long long tick;
  long frameIdx;
  double time;
  double res;
  double svr_confidence;
  double *probEstim; // a copy of the probEstim memory... must be freed
  int nClasses;
  double dur;
  int isFinal;
  int ID;  // custom ID field, 32-bit, usually read from tmeta->metadata.iData[1]

  lsvmDataFrame() : isLast(0), svr_confidence(0.0), probEstim(nullptr), dur(0.0), isFinal(1)  {}

  lsvmDataFrame(svm_node *_x, long _dataSize, int _modelchoice, long long _tick, long _frameIdx, double _time, double _res, int doProbEstim, int _nClasses, double _dur=0.0, int _isFinal=1, int _ID = 0) :
    isLast(0),dataSize(_dataSize), modelchoice(_modelchoice), tick(_tick), 
    frameIdx(_frameIdx), time(_time), res(_res), svr_confidence(0.0), 
    nClasses(_nClasses), dur(_dur), isFinal(_isFinal),  ID(_ID)
  {
    // copy input data
    if (_x!=nullptr) {
      x = (svm_node*)malloc(dataSize);
      memcpy(x,_x,dataSize);
    } else {
      x = nullptr;
    }
    // copy probEstim, if not nullptr
    if (doProbEstim) {
      probEstim = (double *) malloc(nClasses*sizeof(double));
    } else {
      probEstim = nullptr;
    }
    //  memcpy(probEstim,_probEstim,nClasses*sizeof(double));
  }

  void setLast() { isLast = 1; }

  ~lsvmDataFrame() { 
    if (x != nullptr) free(x);
    if (probEstim != nullptr) free(probEstim); 
  }
};


// STL includes for the queue
#include <queue>

// a queue of data frames
typedef std::queue<lsvmDataFrame *> lsvmDataFrameQueue;

// a model wrapper containing, the model itself, the scaling, the feature selection and the class labels, etc.
class svmModelWrapper {
public:
  svmModelWrapper(int pred=0) :
      isLibLinearModel(0), modelResultName(nullptr),
      modelFile(nullptr), scaleFile(nullptr), fselectionFile(nullptr), classesFile(nullptr),
      templ(nullptr), model(nullptr), 
#ifdef BUILD_LIBLINEAR
      modelLinear(nullptr),
#endif
      predictProbability(pred),
      labels(nullptr),
      alienScale(0), scale(nullptr),
      alienClassnames(0), classNames(nullptr),
      nIgnoreEndSelection(0),       
      alienFselection(0), fselection(nullptr)
  {}

  ~svmModelWrapper() {
    int i;
    if (model != nullptr) {
      svm_destroy_model(model);
    }
#ifdef BUILD_LIBLINEAR
    if (modelLinear != nullptr) {
      liblinear_free_and_destroy_model(&modelLinear);
    }
#endif
    if (labels != nullptr) free(labels);
    //if (probEstimates != nullptr) free(probEstimates);

    if ((!alienScale)&&(scale != nullptr)) svm_destroy_scale(scale);
    if ((!alienClassnames)&&(classNames != nullptr)) {
      for (i=0; i<nClassNames; i++) { if (classNames[i] != nullptr) free(classNames[i]); }
      free(classNames);
    }
    if ((!alienFselection)&&(fselection != nullptr)) {
      if (fselection->outputSelIdx.enabled != nullptr) { 
        free(fselection->outputSelIdx.enabled);
      }
      if (fselection->outputSelIdx.map != nullptr) { 
        free(fselection->outputSelIdx.map);
      }
      int n;
      if (fselection->outputSelStr.names != nullptr) {
        for(n=0; n<fselection->outputSelStr.n; n++) {
          if (fselection->outputSelStr.names[n] != nullptr) free(fselection->outputSelStr.names[n]);
        }                             
        free( fselection->outputSelStr.names );
      }
      free(fselection);
    }
  }

  void setTemplate(svmModelWrapper *t) {
    templ = t;
  }

  int loadClasses( const char *file, char *** names );
  int loadSelection( const char *selFile, sFselectionData **fselections );
  int load(); // load everything (model, scale, ...)

  int isLibLinearModel;
  const char *modelResultName;
  const char *modelFile;
  const char *scaleFile;
  const char *fselectionFile;
  const char *classesFile;
  svmModelWrapper *templ;  

  struct svm_model * model;
#ifdef BUILD_LIBLINEAR
  struct liblinear_model * modelLinear;
#endif
  int nClasses, svmType, predictProbability;
  //double * probEstimates;
  int *labels;
  
  int alienScale; // a "1" indicates that we don't have our own scale object and use the 0'th object for all models
  struct svm_scale * scale; 
  int alienClassnames; // a "1" indicates that we don't have our own class name list and use the 0'th object for all models
  char ** classNames;
  int nClassNames;
  int nIgnoreEndSelection; // number of elements to ignore from end of selection list
  int noVerify;

  int alienFselection; // a "1" indicates that we don't have our own class name list and use the 0'th object for all models
  sFselectionData *fselection;
};


class  cLibsvmLiveSink : public cDataSink {
private:
  int sendResult;
  int predictProbability;
  int classifierThreadBusy;
  int bgThreadPriority;

  int saveResult;
  const char * resultFile;

  int forceScale;
  const char * resultRecp;
  const char * resultMessageName;
  //const char **fselection;//, *classes;
  //const char *scalefile, *modelfile;;
  char **modelarray;
  char **classesarray;
  char **scalearray;

  sClassifierResults resCache; // for batchMode

  svmModelWrapper *models;
  int nModels;
  int nScales, nFselections, nClassFiles;
  int currentModel;
  int multiModelMode;
  int batchMode;
  int singlePreprocessMultiModel;

  //long nCls;
  //char ** classNames;
  //int classnames_run;

  lsvmDataFrameQueue *dataFrameQue;
  int threadRunning;
  int loadModelBg;
  int modelLoaded;
  int useThread;
  int threadQueSize;

  int abortLater;
  int noVerify;
  int nIgnoreEndSelection;

  //int buildEnabledSelFromNames(long N, const FrameMetaInfo *fmeta, int index);
  int buildEnabledSelFromNames(long N, const FrameMetaInfo *fmeta, sFselectionData *fselection);
  //int loadSelection( const char *selFile, sFselectionData **fsel );
  //int loadClasses( const char *file, char *** names );
  int loadClassifier();

  void processDigestFrame(lsvmDataFrame * f, int modelIdx);
  void digestFrame(lsvmDataFrame * f, int modelIdx);

  struct svm_node * preprocessFrame(int modelIdx, cVector *vec);

protected:
  SMILECOMPONENT_STATIC_DECL_PR
  int printResult;
  int printParseableResult;
  smileCond dataCond;
  smileMutex dataMtx, runningMtx;
  smileThread bgThread;

  virtual void fetchConfig();
  //virtual int myConfigureInstance();
  virtual int myFinaliseInstance();
  virtual int myTick(long long t);
  //
  virtual int processComponentMessage( cComponentMessage *_msg );

  int queFrameToClassify(lsvmDataFrame *fr);

  //virtual void processResult(long long tick, long frameIdx, double time, float res, double *probEstim, int nClasses, double dur=0.0, int isFinal=1);
  virtual void processResult(lsvmDataFrame *f, int modelIdx, int mmm /* multiModelMode */);

public:
  //static sComponentInfo * registerComponent(cConfigManager *_confman);
  //static cSmileComponent * create(const char *_instname);
  SMILECOMPONENT_STATIC_DECL


  cLibsvmLiveSink(const char *_name);
  void classifierThread();

  virtual ~cLibsvmLiveSink();
};


#endif // BUILD_LIBSVM

#endif // __CLIBSVMLIVESINK_HPP
