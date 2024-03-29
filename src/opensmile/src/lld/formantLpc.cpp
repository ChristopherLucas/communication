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

This component computes Formant canditates and their bandwidths from LPC coefficients

Note: you must downsample the signal to max formant frequency prior to LPC for best results
      use Burg's LPC method (? is the result different from autocorr method ?)

      use 50ms frames with gaussian window and 0.97 pre/de (?) emphasis

*/


#include <lld/formantLpc.hpp>
#include <math.h>
#include <smileutil/zerosolve.h>

#define MODULE "cFormantLpc"

SMILECOMPONENT_STATICS(cFormantLpc)

SMILECOMPONENT_REGCOMP(cFormantLpc)
{
  SMILECOMPONENT_REGCOMP_INIT
  scname = COMPONENT_NAME_CFORMANTLPC;
  sdescription = COMPONENT_DESCRIPTION_CFORMANTLPC;

  // we inherit cVectorProcessor configType and extend it:
  SMILECOMPONENT_INHERIT_CONFIGTYPE("cVectorProcessor")

  SMILECOMPONENT_IFNOTREGAGAIN(
    ct->setField("nFormants","The maximum number of formants to detect (set to < 0 to automatically detect the maximum number of possible formants (nLpcCoeff - 1)",-1);  
    ct->setField("saveFormants","If set to 1, output formant frequencies [field name: formantFreqLpc]",1);  
    ct->setField("saveIntensity","If set to 1, output formant frame intensity [field name: formantFrameIntensity]",0);  
    ct->setField("saveNumberOfValidFormants","If set to 1, output the number of valid formants [field name: nFormants]",0);  
    ct->setField("saveBandwidths","If set to 1, output formant bandwidths [field name: formantBandwidthLpc]",0);
    ct->setField("minF","The minimum of the formant frequency search range",50.0);
    ct->setField("maxF","The maximum detectable formant frequency",5500.0);
    ct->setField("useLpSpec","Experimental option: If set to 1, computes the formants from peaks found in the 'lpSpectrum' field instead of root solving the lpc coefficient polynomial",0);
    ct->setField("medianFilter","1 = enable formant post processing by a median filter of length 'medianFilter' (recommended: 5) (will be rounded up to the next odd number); 0 to disable median filter.",0);
    ct->setField("octaveCorrection","Experimental option: 1 = prevent formant octave jumps (esp. when medianFilter is enabled) by employing simple 'octave' correction. 0 = no correction.",0);

    //ct->setField("margin","minimum formant frequency",50.0);
    ct->setField("processArrayFields",nullptr,0);
  )
  SMILECOMPONENT_MAKEINFO(cFormantLpc);
}

SMILECOMPONENT_CREATE(cFormantLpc)

//-----

cFormantLpc::cFormantLpc(const char *_name) :
  cVectorProcessor(_name),
  nSmooth(5),  
  lpc(nullptr),  roots(nullptr), 
  formant(nullptr), bandwidth(nullptr)
{

}

void cFormantLpc::fetchConfig()
{
  cVectorProcessor::fetchConfig();

  maxF=getDouble("maxF");
  SMILE_IDBG(2,"maxF = %f",maxF); 

  minF=getDouble("minF");
  SMILE_IDBG(2,"minF = %f",minF); 

  nFormants=getInt("nFormants");
  SMILE_IDBG(2,"nFormants = %i",nFormants); 

  saveNumberOfValidFormants=getInt("saveNumberOfValidFormants");
  SMILE_IDBG(2,"saveNumberOfValidFormants = %i",saveNumberOfValidFormants); 

  saveFormants=getInt("saveFormants");
  SMILE_IDBG(2,"saveFormants = %i",saveFormants); 

  saveBandwidths=getInt("saveBandwidths");
  SMILE_IDBG(2,"saveBandwidths = %i",saveBandwidths); 

  saveIntensity=getInt("saveIntensity");
  SMILE_IDBG(2,"saveIntensity = %i",saveIntensity); 

  useLpSpec=getInt("useLpSpec");
  SMILE_IDBG(2,"useLpSpec = %i",useLpSpec); 

  medianFilter=getInt("medianFilter");

  octaveCorrection=getInt("octaveCorrection");
  SMILE_IDBG(2,"octaveCorrection = %i",octaveCorrection); 

  if (medianFilter > 1) {
    nSmooth = medianFilter;
    if ((nSmooth & 1) == 0) nSmooth++;
    SMILE_IDBG(2,"medianFilter = %i",nSmooth); 
  }
  

}

/* setup output vector */
//int cFormantLpc::setupNamesForField(int i, const char*name, long nEl)
int cFormantLpc::setupNewNames(long nEl)
{
  int n=0;

  findInputFields();

  if (saveIntensity) {
    writer_->addField("formantFrameIntensity", 1);
    n+=1;
  }
  if (saveNumberOfValidFormants) {
    writer_->addField( "nFormants", 1 );
    n += 1;
  }
  if (saveFormants) {
    writer_->addField( "formantFreqLpc", nFormants, 1);
    n += nFormants;
  }
  if (saveBandwidths) {
    writer_->addField( "formantBandwidthLpc", nFormants, 1);
    n += nFormants;
  }

  const sDmLevelConfig *c = reader_->getLevelConfig();
  T = (double)(c->basePeriod);

  namesAreSet_=1;
  return n;
}

void cFormantLpc::findInputFields()
{
  const FrameMetaInfo * fmeta = reader_->getFrameMetaInfo();

  lpcCoeffIdx = fmeta->findFieldByPartialName( "lpcCoeff" ); 
  if (lpcCoeffIdx < 0) { 
    lpcCoeffIdx = 0; // default : fallbak to 0th field
    SMILE_IWRN(1,"no 'lpcCoeff' field found in input (this is required!). Using 0th field by default!!");
  } 
  nLpc = fmeta->field[lpcCoeffIdx].N;
  // convert field index to element index (as found in vector later..)
  lpcCoeffIdx = fmeta->fieldToElementIdx( lpcCoeffIdx );

  if (lpcCoeffIdx < 0) {
    SMILE_IERR(1,"unknown error while converting field index (lpcCoeff) to element index (return value: %i)",lpcCoeffIdx); 
    lpcCoeffIdx = 0;
  }

  if (nFormants > nLpc-1) {
    SMILE_IERR(1,"nFormants > nLpcCoeffs-1 , this is not feasible! Setting nFormants = nLpc-1 (%i).",nLpc-1);
    nFormants = nLpc-1;
  }

  if (nFormants <= 0) {  // auto determine nFormants from number of input lpc coefficients
    nFormants = nLpc-1;
  }

  lpcGainIdx = fmeta->findFieldByPartialName( "lpGain" );
  if (lpcGainIdx < 0) {
    if (saveIntensity) SMILE_IERR(1,"lpGain not found as input field, cannot compute formant frame intensity, disabling it now!");
    saveIntensity = 0;
  }
  lpcGainIdx = fmeta->fieldToElementIdx( lpcGainIdx );

  if (useLpSpec) {
    lpSpecIdx = findField("lpSpectrum", 0, &lpSpecN);
  }
}


int cFormantLpc::processVectorInt(const INT_DMEM *src, INT_DMEM *dst, long Nsrc, long Ndst, int idxi)
{
  // not yet implemented
  return 0;
}


int cFormantLpc::processVectorFloat(const FLOAT_DMEM *src, FLOAT_DMEM *dst, long Nsrc, long Ndst, int idxi) // idxi=input field index
{
  long i,j;
  if (lpc == nullptr) lpc = (double*)malloc(sizeof(double)*(nLpc+1));
  if (roots == nullptr) roots = (double*)malloc(sizeof(double)*(nLpc)*2);
  if (formant == nullptr) {
    formant = (double*)calloc(1, sizeof(double)*(nFormants)*(nSmooth+1));
  }
  if (saveBandwidths && (bandwidth == nullptr)) {
    bandwidth = (double*)calloc(1, sizeof(double)*(nFormants)*(nSmooth+1));
  }

  if (lpcCoeffIdx + nLpc > Nsrc) {
    SMILE_IERR(1,"lpcCoeffIdx (%i) + #lpc (%i) > Nsrc (%i) ! ",lpcCoeffIdx,nLpc,Nsrc);
    nLpc = Nsrc - lpcCoeffIdx;
  }
  // get lpc gain, if present:
  double gain = -1.0;
  if (lpcGainIdx >= 0) {
    gain = src[lpcGainIdx];
    //printf("gain : %f idx %i %i\n",gain,lpcGainIdx,lpcCoeffIdx);
  }

  int nValidF=0;

  if (useLpSpec) { /* formant computation by peak picking in lp spectrum and interpolation */
    if (lpSpecIdx >= 0) {
      const FLOAT_DMEM *ls = src+lpSpecIdx;
      int f = 0;
      //double lastMin = -1.0;
      for (i=0; i<nFormants; i++) {
        formant[i] = 0.0;
        if (bandwidth != nullptr) bandwidth[i] = 0.0;
      }

      double deltaIf;
      if (T > 0.0) deltaIf = 1.0/(T*lpSpecN*2.0);
      else {
        SMILE_IERR(1,"T <= 0.0 ! cannot compute lpSpectrum frequencies, formant frequencies will be incorrect!");
        deltaIf = 1.0;
      }
      for (i=2; i<lpSpecN-2; i++) { // find lp spectrum peaks
        if ( (ls[i-1] < ls[i])&&(ls[i]>ls[i+1]) ) { // max:
          // interpolate peak (quadratic):
          double x1 = deltaIf*(double)(i-1); 
          double x2 = deltaIf*(double)(i);
          double x3 = deltaIf*(double)(i+1);
          double yE=0.0; double aQ=0.0;
          double pk = smileMath_quadFrom3pts(x1, ls[i-1], x2, ls[i], x3, ls[i+1], &yE, &aQ);
          if ( (pk >= minF) && (pk <= maxF) && (aQ < 0) && (f<nFormants) ) {
            formant[f] = pk;
            if (bandwidth != nullptr) {
              // compute bandwidth from parameter a:
              bandwidth[f] = 2.0 * ( pk + sqrt((yE/sqrt(2.0) - yE)/aQ) );
            }
            f++;
          }
        } 
      }
      nValidF = f;
    } else { 
      SMILE_IERR(1,"lpSpecIdx < 0 ! no input defined!");
      return 0; 
    }

  
  } else { /*  formant computation from roots of lpc polynomial  */


    // copy lpc coefficients (inverse order) : lpc coefficients -> polynomial coefficients
    for (i=0; i<nLpc; i++) {
      lpc[i] = -src[lpcCoeffIdx+nLpc-i-1]; // lpcCoeffIdx+i
      //printf("lpcpoly[%i]=%f\n",i,lpc[i]);
    }
    lpc[nLpc] = 1.0; // first coefficient is always one

    // get roots (0s) of lpc polynomial
    sZerosolverPolynomialComplexWs * ws = zerosolverPolynomialComplexWorkspaceAllocate(nLpc+1);
    zerosolverPolynomialComplexSolve(lpc, nLpc+1, ws, roots);
    zerosolverPolynomialComplexWorkspaceFree(ws);

    // fix roots to inside the unit circle
    smileMath_complexIntoUnitCircle(roots,nLpc);

    // compute formants and bandwidths from roots
    //for (i=0; i<(nLpc)*2; i+=2) {
      //printf("T %f root[%i]=%f + i * %f\n",T,i/2,roots[i],roots[i+1]);
    //}
    nValidF = smileDsp_lpcrootsToFormants(roots, nLpc, formant, bandwidth, nFormants, T, minF, maxF);

    // sort formants and bandwidths in formant frequency ascending order:
    int nFnon0 = 0;
    for (i=0; i<nFormants; i++) {
      if (formant[i] == 0.0) break;
    }
    nFnon0 = i;
    for (i=0; i<nFnon0; i++) {
      for (j=i+1; j<nFnon0; j++) {
        if (formant[j] < formant[i]) { // swap
          double f = formant[j];
          formant[j] = formant[i];
          formant[i] = f;
          if (bandwidth != nullptr) {
            double b = bandwidth[j];
            bandwidth[j] = bandwidth[i];
            bandwidth[i] = b;
          }
        }
      }
    }

  }


  if (saveFormants) {

    // TODO: correct consideration of bandwidths when sorting for median and when swapping!!

    if (octaveCorrection) {
      // formant octave correction:
      for (i=0; i<nFormants-1; i++) {
        // formant[nFormants+i]; formant[2*nFormants+i];
        if ( fabs((formant[nFormants+i+1] / formant[2*nFormants+i])-1.0) < 0.2 ) {
          formant[i+1] = formant[nFormants+i+1] = formant[2*nFormants+i+1]; // use previous value
        }
        if ( fabs((formant[nFormants+i] / formant[2*nFormants+i+1])-1.0) < 0.2 ) {
          formant[i] = formant[nFormants+i] = formant[2*nFormants+i]; // use previous value
        }
      }
    }

    if (medianFilter) {
      // formant median smoothing:
      double * tmp = new double[nSmooth];
      for (i = 0; i < nSmooth; i++) {
        tmp[i] = 0.0;
      }
      for (i=0; i<nFormants; i++) {
        double min=formant[i+nFormants];
        double max=formant[i+nFormants]; 
        tmp[0] = formant[i+nFormants];
        for (j=2; j<nSmooth; j++) {
          if (formant[j*nFormants+i] > max) max = formant[j*nFormants+i];
          else if (formant[j*nFormants+i] < min) min = formant[j*nFormants+i];
          tmp[j-1] = formant[j*nFormants+i];
        }
        if ((min>0.0)) { // octave jump -> median smoothing
          smileUtil_quickSort_double(tmp,nSmooth);
          formant[i] = tmp[nSmooth/2];
        }
      }
      delete[] tmp;
    }

    if (octaveCorrection || medianFilter) {
      // copy current formant/bandwidth to last
      for (i=0; i<nFormants; i++) {
        for (j=nSmooth-1; j>0; j--) {
          formant[j*nFormants+i] = formant[(j-1)*nFormants+i];
          if (bandwidth != nullptr) bandwidth[j*nFormants+i] = bandwidth[(j-1)*nFormants+i];
        }
      }
    }



  }

  // copy the result to the output vector:
  int n = 0;

  if (saveIntensity) {
    dst[n++] = (FLOAT_DMEM)gain;
  }

  if (saveNumberOfValidFormants) {
    dst[n++] = (FLOAT_DMEM)nValidF;
  }
  if (saveFormants) {
    for (i=0; i<nFormants; i++) {
      dst[n++] = (FLOAT_DMEM)(formant[i]);
      //if (bandwidth != nullptr) printf("formant[%i]=%f  (bw %f)\n",i,(FLOAT_DMEM)(formant[i]),bandwidth[i]);
      //else printf("formant[%i]=%f \n",i,(FLOAT_DMEM)(formant[i]));
    }
  }
  if (saveBandwidths) {
    for (i=0; i<nFormants; i++) {
      dst[n++] = (FLOAT_DMEM)(bandwidth[i]);
    }
  }

  return n;
}


cFormantLpc::~cFormantLpc()
{
  if (lpc != nullptr) free(lpc);
  if (roots != nullptr) free(roots);
  if (formant != nullptr) free(formant);
  if (bandwidth != nullptr) free(bandwidth);
}


/* NEXT: an integrated formant component... */

