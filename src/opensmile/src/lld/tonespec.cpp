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

computes (or rather estimates) semi-tone spectrum from fft spectrum

*/


/*
TODO: 
 *remove normalisation with 1/binsize (or provide option...)
 *no sqrt option by usePower
 *max and rect filter type

 (*tuning correction)
*/

#include <lld/tonespec.hpp>
#include <dsp/dbA.hpp>

#define MODULE "cTonespec"

SMILECOMPONENT_STATICS(cTonespec)

SMILECOMPONENT_REGCOMP(cTonespec)
{
  if (_confman == nullptr) return nullptr;
  int rA = 0;

  sconfman = _confman;
  scname = COMPONENT_NAME_CTONESPEC;
  sdescription = COMPONENT_DESCRIPTION_CTONESPEC;

  // we inherit cVectorProcessor configType and extend it:
  ConfigType *ct = new ConfigType( *(sconfman->getTypeObj("cVectorProcessor")) , scname );
  if (ct == nullptr) {
    SMILE_WRN(4,"cVectorProcessor config Type not found!");
    rA=1;
  }
  if (rA==0) {
    ct->setField("nameAppend", nullptr, "note");
    ct->setField("nOctaves","The number of octaves the spectrum should span",6);
    ct->setField("firstNote","The frequency of the first note (in Hz)",55.0);
    ct->setField("filterType","The shape of the semitone filter:\n   tri (triangular)\n   trp (triangular-powered)\n   gau (gaussian)","gau");
    ct->setField("usePower","Compute the semi-tone spectrum from the power spectrum instead of the magnitudes (= square input values)",0);
    ct->setField("dbA","apply built-in dbA weighting to (power) spectrum (1/0 = yes/no)",1);
  #ifdef DEBUG
    ct->setField("printBinMap","1 = print mapping of fft bins to semi-tone intervals",0);
    ct->setField("printFilterMap","1 = print filter map",0);
  #endif
    ConfigInstance *Tdflt = new ConfigInstance( scname, ct, 1 );
    sconfman->registerType(Tdflt);
  } 

  SMILECOMPONENT_MAKEINFO(cTonespec);
}

SMILECOMPONENT_CREATE(cTonespec)

//-----

cTonespec::cTonespec(const char *_name) :
  cVectorProcessor(_name),
  pitchClassFreq(nullptr),
  binKey(nullptr),
  distance2key(nullptr),
  pitchClassNbins(nullptr),
  filterMap(nullptr),
  flBin(nullptr),
  db(nullptr),
  nOctaves(1),
  nNotes(8),
  usePower(0),
#ifdef DEBUG
  printBinMap(0),
#endif
  filterType(WINF_GAUSS),
  dbA(0)
{

}

void cTonespec::fetchConfig()
{
  cVectorProcessor::fetchConfig();
  
  nOctaves = getInt("nOctaves");
  SMILE_DBG(2,"nOctaves = %i",nOctaves);
  nNotes = nOctaves * 12;
  firstNote = (FLOAT_DMEM)getDouble("firstNote");
  SMILE_DBG(2,"firstNote = %f",firstNote);

  lastNote = firstNote * (FLOAT_DMEM)pow(2.0, (double)nNotes/12.0);

  usePower = getInt("usePower");
  if (usePower) { SMILE_DBG(2,"using power spectrum"); }
  
  dbA = getInt("dbA");
  if (dbA) { SMILE_DBG(2,"dbA weighting for tonespec enabled"); }

  const char *f = getStr("filterType");
  if ( (!strcmp(f,"gau"))||(!strcmp(f,"Gau"))||(!strcmp(f,"gauss"))||(!strcmp(f,"Gauss"))||(!strcmp(f,"gaussian"))||(!strcmp(f,"Gaussian")) ) filterType = WINF_GAUSS;
  else if ( (!strcmp(f,"tri"))||(!strcmp(f,"Tri"))||(!strcmp(f,"triangular"))||(!strcmp(f,"Triangular")) ) filterType = WINF_TRIANGULAR;
  else if ( (!strcmp(f,"trp"))||(!strcmp(f,"TrP"))||(!strcmp(f,"Trp"))||(!strcmp(f,"triangular-powered"))||(!strcmp(f,"Triangular-Powered")) ) filterType = WINF_TRIANGULAR_POWERED;
  else if ( (!strcmp(f,"rec"))||(!strcmp(f,"Rec"))||(!strcmp(f,"rectangular"))||(!strcmp(f,"Rectangular")) ) filterType = WINF_RECTANGULAR;
  
  #ifdef DEBUG
  printBinMap = getInt("printBinMap");
  printFilterMap = getInt("printFilterMap");
  #endif
}


int cTonespec::dataProcessorCustomFinalise()
{
  if (namesAreSet_) return 1;
  //Nfi = reader->getLevelNf();

  // allocate for multiple configurations..
  if (pitchClassFreq == nullptr) pitchClassFreq = (FLOAT_DMEM**)multiConfAlloc();
  if (distance2key == nullptr) distance2key = (FLOAT_DMEM**)multiConfAlloc();
  if (filterMap == nullptr) filterMap = (FLOAT_DMEM**)multiConfAlloc();
  if (binKey == nullptr) binKey = (int**)multiConfAlloc();
  if (pitchClassNbins == nullptr) pitchClassNbins = (int**)multiConfAlloc();
  if (flBin == nullptr) flBin = (int**)multiConfAlloc();
  if ((dbA)&&(db==nullptr)) db = (FLOAT_DMEM**)multiConfAlloc();

  return cVectorProcessor::dataProcessorCustomFinalise();
}

/*
int cTonespec::configureWriter(const sDmLevelConfig *c)
{
  if (c==nullptr) return 0;
  
  // you must return 1, in order to indicate configure success (0 indicated failure)
  return 1;
}
*/

void cTonespec::setPitchclassFreq(int idxc)
{
  FLOAT_DMEM *_pitchClassFreq = pitchClassFreq[idxc];

  double n = 0.0;
  int i;

  if (_pitchClassFreq != nullptr) free(_pitchClassFreq);
  _pitchClassFreq = (FLOAT_DMEM *)malloc(sizeof(FLOAT_DMEM) * (nNotes+2));

  // "firstNote - 1"
  FLOAT_DMEM firstNote0 = firstNote / (FLOAT_DMEM)pow (2.0,1.0/12.0);
  _pitchClassFreq[0] = firstNote0;
  // standard pitch frequencies
  for (i = 1; i < nNotes+2; i++) {
    n += 1.0;
    _pitchClassFreq[i] = firstNote0 * (FLOAT_DMEM)pow (2.0,n/12.0);
  }
  
  pitchClassFreq[idxc] = _pitchClassFreq;
}



void cTonespec::computeFilters(long blocksize, double frameSizeSec, int idxc)
{
  FLOAT_DMEM *_distance2key = distance2key[idxc];
  FLOAT_DMEM *_filterMap = filterMap[idxc];
  FLOAT_DMEM *_pitchClassFreq = pitchClassFreq[idxc];

  int * _binKey = binKey[idxc];
  int * _pitchClassNbins = pitchClassNbins[idxc];

  if (_distance2key != nullptr) free(_distance2key);
  _distance2key = (FLOAT_DMEM*)malloc(sizeof(FLOAT_DMEM) * blocksize);
  if (_binKey != nullptr) free(_binKey);
  _binKey = (int *)malloc(sizeof(int) * blocksize);
  if (_pitchClassNbins != nullptr) free(_pitchClassNbins);
  _pitchClassNbins = (int *)calloc(1,sizeof(int) * (nNotes+2));
  if (_filterMap != nullptr) free(_filterMap);
  _filterMap = (FLOAT_DMEM *)malloc(sizeof(FLOAT_DMEM) * blocksize);

  if (flBin[idxc] != nullptr) free(flBin[idxc]);
  flBin[idxc] = (int*)calloc(1,sizeof(FLOAT_DMEM)*2);
  int firstBin = *(flBin[idxc]);
  int lastBin = *(flBin[idxc]+1);

  int i,b;

  FLOAT_DMEM F0 = (FLOAT_DMEM)(1.0/frameSizeSec);
  if (dbA) {
    if (db[idxc] != nullptr) free(db[idxc]);
    db[idxc] = (FLOAT_DMEM*)malloc(sizeof(FLOAT_DMEM)*blocksize);
    computeDBA( db[idxc], blocksize, F0 );
  }

  firstBin = (int)ceil((_pitchClassFreq[0]+_pitchClassFreq[1]) / (2.0*F0));
  lastBin = (int)floor((_pitchClassFreq[nNotes]+_pitchClassFreq[nNotes+1]) / (2.0*F0));
  //lastBin = (int)round(lastNote / F0);
  if (firstBin < 1) firstBin = 1;
  if (lastBin >= blocksize) lastBin = blocksize-1;
  SMILE_DBG(2,"\tOne bin represents: %f Hz\n\t\t\t\tUsing FFT bin %i to %i\n\t\t\t\tFor freq %f Hz to %f Hz",F0,firstBin,lastBin,firstNote,lastNote);

  int curNote = 0;
  FLOAT_DMEM distance0 = 0.0;
  FLOAT_DMEM distance1 = 0.0;
  for (i = 0 ; i < blocksize; i++)  // checks the frequency of every FFT bin and maps it to a key frequency
  {
    if (curNote > nNotes) curNote=nNotes; // ???
    distance0 = fabs(_pitchClassFreq[curNote] - ((FLOAT_DMEM)i * F0));   //*get the distance
    int note1 = curNote;
    distance1 = fabs(_pitchClassFreq[++note1] - ((FLOAT_DMEM)i * F0));   //*get the distance
    while (distance0 > distance1) {
      //Rprintf("curN: %i  n1 %i - di0 %f  di1 %f\n",curNote,note1,distance0,distance1); 
      if (note1 > nNotes) break;
      distance0 = distance1;
      distance1 = fabs(_pitchClassFreq[++note1] - ((FLOAT_DMEM)i * F0));   //*get the distance
    }
    curNote = note1-1;
    _binKey[i] = curNote;
  }
/*
    _distance2key[i] = 999999.0;
    _binKey[i] = -1;
    for (b=0; b < nNotes; b++) // foreach bin
    {
        //SMILE_PRINT("   note %i (f=%f)",b,_pitchClassFreq[b]);
      distance = _pitchClassFreq[b] - ((FLOAT_DMEM)i * F0);   //*get the distance
      if (distance < 0.0) distance = (-1.0) * distance;
      if (distance < _distance2key[i])
      {
        _binKey[i] = b;   //maps bin to a note (b)
        _distance2key[i] = distance; 
      }
    }
  }
*/
  #ifdef DEBUG
  if (printBinMap) {
    for(i=0; i<blocksize;i++) {
      SMILE_PRINT("   xbin %i (f=%f) -> note %i (f=%f) ",i,(FLOAT_DMEM)i*F0,_binKey[i], _pitchClassFreq[_binKey[i]]);
    }
  }
  #endif

  //Checks how many FFT bins belong to a certain pitchclass
  //used to get the mean power
  for (i=firstBin; i <= lastBin; i++) {
    if (_binKey[i] >= 0) {
      _pitchClassNbins[_binKey[i]]++;
    }
  }
  #ifdef DEBUG
  if (printBinMap) {
    for(i=0; i<nNotes; i++) {
      SMILE_PRINT("   _pitchClassNbins[%i]=%i ",i, _pitchClassNbins[i]);
    }
  }
  #endif

  //set up the filter map
  for (i=0; i < blocksize; i++) _filterMap[i] = 0.0;


  float start_bin, end_bin, middle_bin;
  int i_start_bin, i_end_bin, i_middle_bin;
  float start_freq, end_freq;

  if (filterType != WINF_RECTANGULAR) {
    for (b = 1; b < nNotes - 1; b++) {
      start_freq = (_pitchClassFreq[b - 1] + _pitchClassFreq[b]) / (FLOAT_DMEM)2.0;
      end_freq = (_pitchClassFreq[b] + _pitchClassFreq[b+1]) / (FLOAT_DMEM)2.0;

      start_bin  = start_freq / F0;
      end_bin  = end_freq / F0;

      if ((int)(floor(end_bin) - floor(start_bin)) != _pitchClassNbins[b]) { SMILE_WRN(1,"pitchClassN mismatch %i <> %i (note %i)", (int)round(end_bin - start_bin), _pitchClassNbins[b], b); }

      middle_bin = (_pitchClassFreq[b]/F0);

      i_start_bin  = (int)(ceil(start_bin));  //??????
      i_end_bin  = (int)(floor(end_bin));             //??????
      i_middle_bin = (int)round(_pitchClassFreq[b]/F0);  //??????

      if (i_start_bin > i_end_bin) continue;

//      printf("n=%i -- sF %.3f, eF %.3f, sb=%.2f, eb=%.2f mb=%.2f\n",b,start_freq,end_freq,start_bin,end_bin,middle_bin);

//      if (i_start_bin
      if (i_end_bin >= blocksize) i_end_bin = blocksize-1;
      if (i_start_bin >= blocksize) i_start_bin = blocksize-1;
      if (i_start_bin < 1) i_start_bin = 1;

      if ((filterType == WINF_TRIANGULAR)||(filterType == WINF_TRIANGULAR_POWERED)) {
        for (i=i_start_bin; i < i_middle_bin; i++) {
          _filterMap[i] = ((FLOAT_DMEM)1.0 - ((middle_bin - FLOAT_DMEM(i))/(middle_bin - start_bin)) );
          if (_filterMap[i] > (FLOAT_DMEM)1.0) _filterMap[i] = (FLOAT_DMEM)2.0 - _filterMap[i];
        }
        for (i=i_middle_bin; i <= i_end_bin; i++) {
          _filterMap[i] = ((FLOAT_DMEM)1.0 - ((FLOAT_DMEM(i) - middle_bin)/(end_bin - middle_bin)) );
          if (_filterMap[i] > (FLOAT_DMEM)1.0) _filterMap[i] = (FLOAT_DMEM)2.0 - _filterMap[i];
        }
      }

      if (filterType == WINF_GAUSS) {
        double x_val;
        double delta;
        double dist_val;
        for (i=i_start_bin; i <= i_end_bin; i++) {
          dist_val = (double) (end_bin - start_bin);
          if (dist_val > 0.0) {
            x_val = (double)((double)i - middle_bin);
            delta = dist_val / 15.0;   // M_PI  // ?????
            _filterMap[i] = (FLOAT_DMEM)( (10.0 / 4.0) * (1.0 / sqrt(2.0*M_PI)) * exp( -0.5 * (1.0/delta) * (1.0/delta) * pow(x_val,2.0)) );
          }
        }
      }

    }
  }

  if (filterType == WINF_TRIANGULAR_POWERED) {
    for (i = 0; i < blocksize; i++) {
      _filterMap[i] *= _filterMap[i];
    }
  }

#ifdef DEBUG
  if (printFilterMap) {
    printf("\nfirstBin: %i  -- lastBin: %i\n",firstBin,lastBin);
    printf("\nfilterMap:\n");
    for (i=0; i < blocksize; i++) {
      printf("%.4f",_filterMap[i]);
      if (i<blocksize-1) printf(",");
    }
    printf("\n\n");
  }
#endif

  //Reduce bins to "first_bin <-> last_bin"
  for (i = 0; i < firstBin; i++) {
    _filterMap[i] = 0;
  }
  for (i = lastBin+1; i < blocksize; i++) {
    _filterMap[i] = 0;
  }

  if (dbA) {  // optional dbA
    FLOAT_DMEM *d = db[idxc];
    for (i = firstBin; i <= lastBin; i++) {
      _filterMap[i] *= *(d++);
    }
  }

  *(flBin[idxc]) = firstBin;
  *(flBin[idxc]+1) = lastBin;
  distance2key[idxc] = _distance2key;
  filterMap[idxc] = _filterMap;
  binKey[idxc] = _binKey;
  pitchClassNbins[idxc] = _pitchClassNbins;
}


int cTonespec::setupNamesForField(int i, const char*name, long nEl)
{
  const sDmLevelConfig *c = reader_->getLevelConfig();
  setPitchclassFreq(getFconf(i));
  computeFilters(nEl, c->frameSizeSec, getFconf(i));

  return cVectorProcessor::setupNamesForField(i,"tone",nNotes);
}


/*
int cTonespec::myFinaliseInstance()
{
  int ret;
  ret = cVectorProcessor::myFinaliseInstance();
  return ret;
}
*/

// a derived class should override this method, in order to implement the actual processing
/*
int cTonespec::processVectorInt(const INT_DMEM *src, INT_DMEM *dst, long Nsrc, long Ndst, int idxi) // idxi=input field index
{
  // do domething to data in *src, save result to *dst
  // NOTE: *src and *dst may be the same...
  
  return 1;
}
*/

// a derived class should override this method, in order to implement the actual processing
int cTonespec::processVectorFloat(const FLOAT_DMEM *src, FLOAT_DMEM *dst, long Nsrc, long Ndst, int idxi) // idxi=input field index
{
  int i;

  idxi=getFconf(idxi);
  FLOAT_DMEM *_distance2key = distance2key[idxi];
  FLOAT_DMEM *_filterMap = filterMap[idxi];
  int * _binKey = binKey[idxi];
  int * _pitchClassNbins = pitchClassNbins[idxi];
  int firstBin = *(flBin[idxi]);
  int lastBin = *(flBin[idxi]+1);


  // copy & square the fft magnitude
  FLOAT_DMEM *_src;
  if (usePower) {
    _src = (FLOAT_DMEM*)malloc(sizeof(FLOAT_DMEM)*Nsrc);
    if (src==nullptr) OUT_OF_MEMORY;
    for (i=0; i<Nsrc; i++) {
      _src[i] = src[i]*src[i];
    }
    src = _src;
  }

  // do the tone filtering by multiplying with the filters and summing up
  bzero(dst, Ndst*sizeof(FLOAT_DMEM));

  // Sum the FFT bins for each pitch class and compute mean value
  for (i=firstBin; i <= lastBin; i++) {
    if ((_binKey[i] > 0) && (_binKey[i] <= nNotes)) {
      dst[_binKey[i]-1] += src[i] * _filterMap[i];
    }
  }

  for (i = 0; i < nNotes; i++) {
    if (_pitchClassNbins[i+1] > 0) {
      dst[i] /= (FLOAT_DMEM)(_pitchClassNbins[i+1]);
      if (usePower) if (dst[i]>=0.0) dst[i] = sqrt(dst[i]); else dst[i] = 0.0; // FIXME ????
    } else dst[i] = 0.0;
  }

  if ((usePower)&&(_src!=nullptr)) free((void *)_src);

  // TODO: semi-tone normalisation per octave!?  (here or in CHROMA?)
  // TODO: check Gaussian filter function... also: filter overlap?
  return 1;
}

cTonespec::~cTonespec()
{
  multiConfFree(pitchClassFreq);
  multiConfFree(pitchClassNbins);
  multiConfFree(binKey);
  multiConfFree(distance2key);
  multiConfFree(flBin);
  multiConfFree(filterMap);
  if (dbA) multiConfFree(db);
}

