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


/*

  central configuration manager for all components
  provides standard interface for various plugable config reader classes (e.g. config file, commandline, database, etc.)
  
  this configManager ONLY manages the configuration of specific parameters for components (or component instances),
  it does NOT manage the components to be initialized and the component dependencies (i.e. initialisation of readers and writers of each component)

  module names in this class refer to the names of the component instances.
  i.e. if you create a component instance config be sure to use the same module (aka. component) names in the component parameter configuration file (this is the one read and managed by this module)

  (however, there is one way to put the component initialisation config in this class:
    you need to create a component "compInitializer"  (or use the componentManager)
    and then configure this component
  )
 */


// TODO::::  implement setValue functions in analogy to getValue functions to override configuration values!
// also, add a flag to setValue, to only overwrite UNset options

#include <core/configManager.hpp>
#include <ctype.h>




#define MODULE "configManager"

// split n at first . into b (left) and s (right)
// return 1 if '.' was found in *n, 0 otherwise  , *b is allocated and must be freed by calling code!
int instNameSplit(const char *n, char **b, const char **s)
{
  if (n!=nullptr) {
    const char *x = strchr(n,'.');
    if (x== nullptr) {
      if (b != nullptr) *b = strdup(n);
      if (s != nullptr) *s = nullptr;
      return 0;
    } else {
      size_t blen = (size_t)(x) - (size_t)(n);
      if (b != nullptr) {
        *b = (char*)malloc(sizeof(char)*(blen+1));
        memcpy(*b,n,blen);
        (*b)[blen]=0;
      }
      if (s != nullptr) *s = x+1;
      return 1;
    }
  }
  return 0;
}

/** Config Instance ***************************/


//#define CFTP_VAR   1000      // experimental...? variable type, determined at loading of config file..


void ConfigValueNum::copyFrom(const ConfigValue *val)
{
  if (val==nullptr) return;
  if (val->getType() != getType()) CONF_MANAGER_ERR("ConfigValue::copyFrom, cannot copy from incompatible type (%i)!",val->getType());
  valueD = val->getDouble();
  valueI = val->getInt();
  set=val->isSet();
}

void ConfigValueStr::copyFrom(const ConfigValue *val)
{
  if (val==nullptr) return;
  if (val->getType() != getType()) CONF_MANAGER_ERR("ConfigValue::copyFrom, cannot copy from incompatible type (%i)!",val->getType());
  if (str != nullptr) free(str);
  if (val->getStr() != nullptr) {
    str = strdup(val->getStr());
    set=val->isSet();
  } else { set = 0; }
}

void ConfigValueChr::copyFrom(const ConfigValue *val)
{
  if (val==nullptr) return;
  if (val->getType() != getType()) CONF_MANAGER_ERR("ConfigValue::copyFrom, cannot copy from incompatible type (%i)!",val->getType());
  c = val->getChar();
  set=val->isSet();
}


void ConfigValueObj::setValue(ConfigInstance * value, int n)
{
  if ((obj != nullptr)&&(freeObj)) delete obj;
  obj = value; if (value!=nullptr) set=1; else set = 0;
}

void ConfigValueObj::copyFrom(const ConfigValue *val)
{
  if (val==nullptr) return;
  if (val->getType() != getType()) CONF_MANAGER_ERR("ConfigValue::copyFrom, cannot copy from incompatible type (%i)!",val->getType());
  if ((obj != nullptr)&&(freeObj)) delete obj;
  obj = val->getObj(); freeObj=0;
  if (obj != nullptr) set=val->isSet(); else set = 0;
}

int ConfigValueObj::missingFrom(const ConfigValue *val)
{
  if (val==nullptr) return 0;
  if (val->getType() != getType()) CONF_MANAGER_ERR("ConfigValue::copyFrom, cannot copy from incompatible type (%i)!",val->getType());
  if (obj != nullptr) {
    return obj->missingFrom(val->getObj());
  } else {
    //obj = val->getObj(); freeObj=0;  // TODO: nicer copy mechanism... e.g. copy constructor...?
    CONF_MANAGER_ERR("ConfigValueObj::missingFrom: ConfigInstance copy not yet supported!");
  }
}

int ConfigValueObj::updateWith(const ConfigValue *val)
{
  if (val==nullptr) return 0;
  if (val->getType() != getType()) CONF_MANAGER_ERR("ConfigValue::copyFrom, cannot copy from incompatible type (%i)!",val->getType());
  if (obj != nullptr) {
    return obj->updateWith(val->getObj());
  } else {
    //obj = val->getObj(); freeObj=0;  // TODO: nicer copy mechanism... e.g. copy constructor...?
    CONF_MANAGER_ERR("ConfigValueObj::updateWith: ConfigInstance copy not yet supported!");
  }
}


ConfigValueObj::~ConfigValueObj()
{
  if ((obj != nullptr)&&(freeObj))
    delete obj;
}


ConfigValueArr::ConfigValueArr(int initN) :el(nullptr), N(0),  maxN(-1)
{
  if (initN < NEL_ALLOC_BLOCK) initN = NEL_ALLOC_BLOCK;
  el = (ConfigValue **)calloc(1,sizeof(ConfigValue*)*initN);
  aName = (char **)calloc(1,sizeof(char*)*initN);
  if (el==nullptr) OUT_OF_MEMORY;
  if (aName==nullptr) OUT_OF_MEMORY;
  N=initN;
  type=CFTP_ARR;
}

ConfigValueArr::~ConfigValueArr()
{
  if (el != nullptr) {
    int i;
    for (i=0; i<N; i++) {
      if (el[i] != nullptr) delete el[i];
    }
    free(el);
  }
  if (aName != nullptr) {
    int i;
    for (i=0; i<N; i++) {
      if (aName[i] != nullptr) free(aName[i]);
    }
    free(aName);
  }
}

void ConfigValueArr::setValue(ConfigValue *v, int n)
{
  // TODO: checks... + realloc if n>=Nalloc
  if (checkIdxWr(n)) {
    if (el[n] != nullptr) delete(el[n]);
    el[n] = v;
    set=1;
  } else CONF_MANAGER_ERR("ConfigValueArr::setValue: invalid index %i (N=%i)",n,N);
}

int ConfigValueArr::checkIdx(int n) const
{
  if (n >= 0) {
    if (n<N) {
      return 1;
    } 
  }
  return 0;
}

int ConfigValueArr::checkIdxWr(int n)
{
  if (n >= 0) {
    if (n<N) {
      if (n>maxN) maxN=n;
      return 1;
    } else { // realloc:
      int newSize = n;
      if (newSize-N < NEL_ALLOC_BLOCK) newSize = N+NEL_ALLOC_BLOCK;
      ConfigValue **_el = (ConfigValue**)crealloc( el, sizeof(ConfigValue*)*newSize, sizeof(ConfigValue*)*N );
      char **_aName = (char**)crealloc( aName, sizeof(char*)*newSize, sizeof(char*)*N );
      if (_el==nullptr) OUT_OF_MEMORY;
      if (_aName==nullptr) OUT_OF_MEMORY;
      N = newSize;
      el = _el;
      aName=_aName;
      if (n>maxN) maxN=n;
      return 1;
    }
  }
  return 0;
}

int ConfigValueArr::findField(const char *_name, int noerr) const
{
  int i;
  if (_name == nullptr) return -2;  // bogus name...
  if (aName == nullptr) return -2; // not an associative array??
  SMILE_DBG(7,"ConfigValueArr::findField: maxN = %i",maxN);
  for (i=0; i<=maxN; i++) {
    SMILE_DBG(7,"ConfigValueArr::findField: comp iter. %i",i);
    if ((el[i] != nullptr)&&(aName[i] != nullptr)) {
      SMILE_DBG(7,"ConfigValueArr::findField: compare '%s' <-> '%s'",aName[i],_name);
      if (!strcmp(aName[i],_name)) { return i; }
    }
  }
  if (!noerr) SMILE_ERR(5,"ConfigValueArr::findField: field '%s' not found in this associative array",_name);
  return -1; // not found
}

int ConfigValueArr::findFieldWr(const char *_name)
{
  int idx = findField(_name,1);
  if (idx == -1) { // not found: assign it...
    int i;
    // search for first available index
    for (i=0; i<N; i++) {
      if ((el[i] == nullptr)&&(aName[i] == nullptr)) {
        idx = i;
        break;
      }
    }
    if (idx == -1) { // no free elements... allocate next:
      idx = N;
      if (!checkIdxWr(N)) idx = -1;  // no space, or some other error...!
    }
    if (idx >= 0) {
      aName[idx] = strdup(_name);
    }
  }
  return idx;
}

ConfigValue * ConfigValueArr::operator[](int n) const
{
  SMILE_DBG(7,"operator [int] of ConfigValueArr called...");
  if (checkIdx(n)) return el[n];
  else return nullptr;
}

ConfigValue * ConfigValueArr::operator[](const char *_name) const
{
  SMILE_DBG(7,"operator [char *] of ConfigValueArr called...");
  int n = findField(_name);
  if (checkIdx(n)) return el[n];
  else return nullptr;
}

// TODO: resize array.....!!!
void ConfigValueArr::copyFrom(const ConfigValue *val)
{
  int i;
  if (val==nullptr) return;
  ConfigValueArr *a;
  if (val->getType() >= CFTP_ARR) a = (ConfigValueArr *)val;
  else CONF_MANAGER_ERR("ConfigValueArr::copyFrom called with non-array element as argument!");
  int Nc;
  Nc = a->getN();
  if (Nc > N) Nc = N;
  for (i=0; i<Nc; i++) {
    if (el[i] != nullptr)
      el[i]->copyFrom(a->el[i]);
    //TODO....  else : REAL copy of ConfigVal object... ??
  }
}

int ConfigValueArr::missingFrom(const ConfigValue *val)
{
  int i;
  if (val==nullptr) return 0;
  ConfigValueArr *a;
  if (val->getType() >= CFTP_ARR) a = (ConfigValueArr *)val;
  else return 0;
  int Nc;
  Nc = a->getN();
  if (Nc > N) Nc = N;
  for (i=0; i<Nc; i++) {
    if (el[i] != nullptr)
      el[i]->missingFrom(a->el[i]);
    //TODO....  else : REAL copy of ConfigVal object... ??
  }
  return 0; // TODO...
}

int ConfigValueArr::updateWith(const ConfigValue *val)
{
  int i;
  if (val==nullptr) return 0;
  ConfigValueArr *a;
  if (val->getType() >= CFTP_ARR) a = (ConfigValueArr *)val;
  else return 0;
  int Nc;
  Nc = a->getN();
  if (Nc > N) Nc = N;
  for (i=0; i<Nc; i++) {
    if (el[i] != nullptr)
      el[i]->updateWith(a->el[i]);
    //TODO....  else : REAL copy of ConfigVal object... ??
  }
  return 0; // TODO...
}



void ConfigValueNumArr::setValue(double v, int n)
{
  if (checkIdxWr(n)) {
    if (el[n] == nullptr) {
      // allocate value..
      el[n] = new ConfigValueNum(v);   // WARNING: freeObj = 1  always!
      if (el[n] == nullptr) OUT_OF_MEMORY;
    } else {
      el[n]->setValue(v);
    }
  }
}

void ConfigValueNumArr::setValue(int v, int n)
{
  if (checkIdxWr(n)) {
    if (el[n] == nullptr) {
      // allocate value..
      el[n] = new ConfigValueNum(v);   // WARNING: freeObj = 1  always!
      if (el[n] == nullptr) OUT_OF_MEMORY;
    } else {
      el[n]->setValue(v);
    }
  }
}

void ConfigValueStrArr::setValue(const char *v, int n)
{
  if (checkIdxWr(n)) {
    if (el[n] == nullptr) {
      // alocate value..
      el[n] = new ConfigValueStr(v);   // WARNING: freeObj = 1  always!
      if (el[n] == nullptr) OUT_OF_MEMORY;
    } else {
      el[n]->setValue(v);
    }
  }
}

void ConfigValueChrArr::setValue(char v, int n) {
  if (checkIdxWr(n)) {
    if (el[n] == nullptr) {
      // alocate value..
      el[n] = new ConfigValueChr(v);   // WARNING: freeObj = 1  always!
      if (el[n] == nullptr) OUT_OF_MEMORY;
    } else {
      el[n]->setValue(v);
    }
  }
}

void ConfigValueObjArr::setValue(ConfigInstance *v, int n) {
  if (checkIdxWr(n)) {
    if (el[n] == nullptr) {
      // alocate value..
      el[n] = new ConfigValueObj(v);   // WARNING: freeObj = 1  always!
      if (el[n] == nullptr) OUT_OF_MEMORY;
    } else {
      el[n]->setValue(v);
    }
  }
}


/**************/


/**** Config Type ***************************************************************/


ConfigType::ConfigType(const char *_name, int N_) :
  I(0),  
  element(nullptr)
{
  if (_name != NULL) setName( _name );
  parentName[0] = 0; // no parent name..
  if (N_ > 0) {
    N=N_;
    element = (ConfigDescription *)calloc(1,sizeof(ConfigDescription)*N_);
  } else {
    CONF_MANAGER_ERR("Cannot create ConfigType object with number of elements _N < 0 (N_=%i)!",N_);
  }
}

// type copy constructor
ConfigType::ConfigType( ConfigType const& copy, const char *_newname) :
  N(copy.N),
  I(copy.I),
  element(nullptr)
{
  if (_newname != nullptr) {
    // save last name as parent name
    memcpy( parentName, name, CONFIGTYPE_STRLEN+1 ); 
    // overwrite with new name
    setName(_newname);
  } else { setName(copy.name); }
  // copy configDescription:
  if (copy.element != nullptr) {
    element = (ConfigDescription *)calloc(1,sizeof(ConfigDescription)*N);
    memcpy( element, copy.element, sizeof(ConfigDescription)*N );
    int i;
    for (i=0; i<N; i++) {
      element[i].description = nullptr;
      element[i].dfltStr = nullptr;
      element[i].freeType=0;
      if (copy.element[i].description != nullptr) {
        element[i].description = strdup(copy.element[i].description);
      }
      if (copy.element[i].dfltStr != nullptr) {
        element[i].dfltStr = strdup(copy.element[i].dfltStr);
      }
    }

  }
}

void ConfigType::setName(const char *_name)
{
  if (_name == nullptr) { SMILE_ERR(1,"cannot set name == nullptr (setName)"); }
  else {
    size_t l = strlen(_name);
    if (l>CONFIGTYPE_STRLEN) {
      l=CONFIGTYPE_STRLEN;
    }
#ifdef _MSC_VER // Visual Studio specific macro
    strncpy_s( name, CONFIGTYPE_STRLEN, _name, MIN((l+1),CONFIGTYPE_STRLEN) );
#else
    if(CONFIGTYPE_STRLEN<l+1)
      strncpy( name, _name, CONFIGTYPE_STRLEN);
    else
      strcpy( name, _name);      
#endif
    name[CONFIGTYPE_STRLEN] = 0; // ensure last element is always the terminating nullptr character
  }
}

int ConfigType::findFieldH(const char *_name, int *n, const ConfigType **tp, int *aIdx, char **aStr) const
{
  try {
  if (_name != nullptr) {
    int h, idx;
    char *base=nullptr;
    const ConfigType *cur=this;
    const char*origname = _name;
    int arrIdx = -1;
    char *arrStr = nullptr;

    do {
      if (cur==nullptr) {
        CONF_MANAGER_ERR("ConfigType::findFieldH: cannot dereference nullptr subobject pointer while finding filed '%s' (rem:'%s')",origname,_name);
      }
      // split name
      const char *rem=nullptr;
      h = instNameSplit(_name, &base, &rem);
      if (base == nullptr) { CONF_MANAGER_ERR("no base name returned by instNameSplit (name='%s')!\n",_name); }
      arrIdx = -1;
      if (arrStr != nullptr) { free(arrStr); arrStr=nullptr; }
      idx = cur->findField(base,&arrIdx,&arrStr);
      if (base!=nullptr) { free(base); base=nullptr; }
      _name=rem;

      if (h==1) {
        if ((idx >= 0)&&(idx < N)) { // check for valid index
          if ((cur->getType(idx) == CFTP_OBJ)||(cur->getType(idx) == CFTP_OBJ_ARR)) {
            cur = cur->element[idx].subType;
          }
          else
            CONF_MANAGER_ERR("ConfigType::findFieldH: subtype object referenced in '%s', however field %i is not of type OBJ or OBJ_ARR!",_name,idx);
        } else {
          // error field "base" not found...
          CONF_MANAGER_ERR("ConfigType::findFieldH: referenced base field with name '%s' not found!",origname);
        }
      }
    } while (h==1);

    if (h==0) {
      if ((idx >= 0)&&(idx < N)) { // check for valid index
        if (n!=nullptr) *n = idx;
        if (tp!=nullptr) *tp = cur;
        if (aIdx!=nullptr) *aIdx = arrIdx;
        if (aStr!=nullptr) *aStr = arrStr;
        else if (arrStr != nullptr) free(arrStr);
        return 1;
      } else
        if (arrStr != nullptr) { free(arrStr); }
        // error field "base" not found...
        CONF_MANAGER_ERR("ConfigType::findFieldH: referenced base field with name '%s' not found!",origname);
    }
      if (arrStr != nullptr) { free(arrStr); }
  }
  } catch (ConfigException *) {}
  return 0;
}

void ConfigType::printTypeHelp(char *basestr, int _subtype) const
{
  int texOutput = 0;/////<---- enable this (=1), recompile, to get almost tex-ready documentation

  if (basestr == nullptr) {
    if (texOutput) {
      SMILE_PRINT("\n %%Type: '%s'\n\\label{sec:reference:componentlist:%s}\n\\begin{description}\n",getName(),getName());
    } else {
      SMILE_PRINT("\n === ConfigType '%s' : ===",getName());
    }
  }
  // TODO: print type description
  if (element != nullptr) {
    int i;
    const char *ArrS="";
    for (i=0; i<I; i++) {
      if (element[i].enabled) {

        if (texOutput) {
          char *b=nullptr;
          if (basestr != nullptr) b=myvprint("%s.",basestr);
          else b=myvprint("");
          if (element[i].type >= CFTP_ARR) { element[i].type -= CFTP_ARR+1; ArrS="[]"; }
          else { ArrS=""; }
          switch(element[i].type) {
            case CFTP_OBJ: 
              if (element[i].subType != nullptr)
                SMILE_PRINT("\\item [%s%s%s = $<$object of type `%s'$>$] See the documentation of `%s' for more information (section~\\ref{sec:reference:componentlist:%s}).",b,element[i].name,ArrS,element[i].subType->getName(),element[i].subType->getName(),element[i].subType->getName());
              break;
            case CFTP_NUM: 
              if (element[i].dfltDouble == floor(element[i].dfltDouble)) {
                SMILE_PRINT("\\item [%s%s%s = $<$numeric$>$] \\hspace{1cm} [Default: %.0f] \\\\",b,element[i].name,ArrS,element[i].dfltDouble);
              } else {
                SMILE_PRINT("\\item [%s%s%s = $<$numeric$>$] \\hspace{1cm} [Default: %g] \\\\",b,element[i].name,ArrS,element[i].dfltDouble);
              } 
              break;
            case CFTP_STR:
              SMILE_PRINT("\\item [%s%s%s = $<$string$>$]  \\hspace{1cm} [Default: `%s'] \\\\",b,element[i].name,ArrS,element[i].dfltStr);
              break;
            case CFTP_CHR: 
              SMILE_PRINT("\\item [%s%s%s = $<$char$>$]   \\hspace{1cm} [Default: `%c'] \\\\",b,element[i].name,ArrS,element[i].dfltChar);
              break;

          }
          if (b!=nullptr) free(b);

          if (element[i].description != nullptr) {
            SMILE_PRINT("   %s",element[i].description);
          }
          if ((element[i].type == CFTP_OBJ)&&(_subtype)) {
            if (element[i].subType != nullptr)
              if (basestr!=nullptr)
                element[i].subType->printTypeHelp(myvprint("%s.%s%s",basestr,element[i].name,ArrS));
              else
                element[i].subType->printTypeHelp(myvprint("%s%s",element[i].name,ArrS));
          }
        
        } else { ///////////////////// default console output:
        
          char *b=nullptr;
          if (basestr != nullptr) b=myvprint("%s.",basestr);
          else b=myvprint("");
          if (element[i].type >= CFTP_ARR) { element[i].type -= CFTP_ARR+1; ArrS="[]"; }
          else { ArrS=""; }
          switch(element[i].type) {
            case CFTP_OBJ: 
              if (element[i].subType != nullptr)
                SMILE_PRINT(" %s%s%s = <object of type '%s'>",b,element[i].name,ArrS,element[i].subType->getName());
              break;
            case CFTP_NUM: 
              if (element[i].dfltDouble == floor(element[i].dfltDouble)) {
                SMILE_PRINT(" %s%s%s = <numeric> [dflt: %.0f]",b,element[i].name,ArrS,element[i].dfltDouble);
              } else {
                SMILE_PRINT(" %s%s%s = <numeric> [dflt: %g]",b,element[i].name,ArrS,element[i].dfltDouble);
              }
              //SMILE_PRINT(" %s%s%s = <numeric> [dflt: %f]",b,element[i].name,ArrS,element[i].dfltDouble);
              break;
            case CFTP_STR:
              SMILE_PRINT(" %s%s%s = <string>  [dflt: '%s']",b,element[i].name,ArrS,element[i].dfltStr);
              break;
            case CFTP_CHR: 
              SMILE_PRINT(" %s%s%s = <char>    [dflt: '%c']",b,element[i].name,ArrS,element[i].dfltChar);
              break;

          }
          if (b!=nullptr) free(b);

          if (element[i].description != nullptr) {
            SMILE_PRINT("   %s",element[i].description);
          }
          if ((element[i].type == CFTP_OBJ)&&(_subtype)) {
            if (element[i].subType != nullptr)
              if (basestr!=nullptr)
                element[i].subType->printTypeHelp(myvprint("%s.%s%s",basestr,element[i].name,ArrS));
              else
                element[i].subType->printTypeHelp(myvprint("%s%s",element[i].name,ArrS));
          }

        }
      }
    }
  }
  if (basestr != nullptr) free(basestr);
  else {
    if (texOutput) {
      SMILE_PRINT("\n \\end{description}\n",getName());
    } else {
      SMILE_PRINT(" ");
    }
  }
}

void ConfigType::printTypeDfltConfig(char *basestr, int _subtype, int withDescription, int ignInternal) const
{
  if (I <= 0) return;

  if (ignInternal) { // check for various internal types such as dataReader/writer and do not print them...
    const char * na = getName(); 
    if (na != nullptr) {
      if (!strcmp(na,"cDataWriter")) {
        if (basestr != nullptr) {
          if (withDescription) {
            SMILE_PRINT("   // Data memory level to write data to.\n   // Only ONE level is possible here, and only this writer may write to that level.");
          }
          SMILE_PRINT("writer.dmLevel = <<XXXX>>");
        }
        return;
      } else if (!strcmp(na,"cDataReader")) {
        if (basestr != nullptr) {
          if (withDescription) {
            SMILE_PRINT("   // Data memory level(s) to read data from. Concat multiple level names using ';'.");
          }
          SMILE_PRINT("reader.dmLevel = <<XXXX>>");
        }
        return;
      } 
      //...

    }
  }

  

  // TODO: print only 'public' components, i..e  no dataReader, etc.

  if (basestr == nullptr) {
    SMILE_PRINT("\n  ;;;; default (template) configuration section for component '%s' ;;;;",getName());

    const char * na = getName(); 
    if (na != nullptr) {
      char * tmp = strdup(na);
      char * tmpMod = tmp+1;
      tmpMod[0] = tolower(tmpMod[0]);
      SMILE_PRINT("[%s:%s]",tmpMod,na);
      free(tmp);
    } else {
      SMILE_PRINT("[null:cnullptr]");
    }

  }

  // TODO: print type description
  if (element != nullptr) {
    int i;
    
    for (i=0; i<I; i++) {
      const char *ArrS="";

      int ok = 1;
      if (ignInternal>=2) {
        if (!strncmp(element[i].name,"blocksize",9) || !strncmp(element[i].name,"buffersize",10)) {
          ok=0;
        }
      }

      if ((element[i].enabled)&&(element[i].printDflt)/*&&(element[i].type < CFTP_ARR)*/&&(ok)) {


        char *b=nullptr;
        if (basestr != nullptr) b=myvprint("%s.",basestr);
        else b=myvprint("");
        
        int etype = element[i].type;
        if (element[i].type >= CFTP_ARR) {
          etype = CFTP_ARR;
          ArrS="[]";
        }

        char *eDescription=nullptr;

        if (withDescription) { //element[i].type -= CFTP_ARR+1; ArrS="[]"; }


          if (element[i].description != nullptr) {
            char * desc = strdup(  element[i].description );
            // parse for number of lines
            long c,C; int nLines = 1;
            long l = (long)strlen(desc);
            for (c=0; c<l; c++) {
              if (desc[c] == '\n') nLines++;
            }
            eDescription = (char *)calloc(1,sizeof(char) * (l + nLines * 6 + 16));
            C=0;
            eDescription[C++] = ' ';
            eDescription[C++] = ' ';
            eDescription[C++] = ' ';
            eDescription[C++] = '/';
            eDescription[C++] = '/';
            eDescription[C++] = ' ';
            for (c=0; c<l; c++) {
              eDescription[C++] = desc[c];
              if (desc[c] == '\n') {
                eDescription[C++] = ' ';
                eDescription[C++] = ' ';
                eDescription[C++] = ' ';
                eDescription[C++] = '/';
                eDescription[C++] = '/';
                eDescription[C++] = ' ';
              } 
            }
            free(desc);
          }

        }
        switch(etype) {
            case CFTP_OBJ: 
              //              if (element[i].subType != nullptr)
              //SMILE_PRINT(" %s%s%s = <object of type '%s'>",b,element[i].name,ArrS,element[i].subType->getName());
              break;
            case CFTP_NUM: 
              if (eDescription != nullptr) SMILE_PRINT("%s",eDescription);
              if (element[i].dfltDouble == 0) {
                SMILE_PRINT("%s%s%s = 0",b,element[i].name,ArrS);
              } else if (element[i].dfltDouble == 0) {
                SMILE_PRINT("%s%s%s = 1",b,element[i].name,ArrS);
              } else {
                // check for int...
                double d = element[i].dfltDouble;
                if (d - floor(d) == 0) { // integer...
                  SMILE_PRINT("%s%s%s = %i",b,element[i].name,ArrS,(long)d);
                } else { // float...
                  SMILE_PRINT("%s%s%s = %f",b,element[i].name,ArrS,element[i].dfltDouble);
                }
              }
              break;
            case CFTP_STR:
              if (element[i].dfltStr != nullptr) {
                if (eDescription != nullptr) SMILE_PRINT("%s",eDescription);
                SMILE_PRINT("%s%s%s = %s",b,element[i].name,ArrS,element[i].dfltStr);
              } else {
                if (eDescription != nullptr) SMILE_PRINT("%s (default: '<nullptr>')",eDescription);
                SMILE_PRINT("// %s%s%s = ",b,element[i].name,ArrS);
              }
              break;
            case CFTP_ARR:
              if (eDescription != nullptr) SMILE_PRINT("%s (default: empty)",eDescription);
              SMILE_PRINT("// %s%s%s = ",b,element[i].name,ArrS);
              break;
            case CFTP_CHR: 
              if (eDescription != nullptr) SMILE_PRINT("%s",eDescription);
              SMILE_PRINT("%s%s%s = %c",b,element[i].name,ArrS,element[i].dfltChar);
              break;

        }
        if (b!=nullptr) free(b);
        if (eDescription != nullptr) free(eDescription);




        if ((element[i].type == CFTP_OBJ)&&(_subtype)) {
          if (element[i].subType != nullptr)
            if (basestr!=nullptr)
              element[i].subType->printTypeDfltConfig(myvprint("%s.%s%s",basestr,element[i].name,ArrS),_subtype,withDescription);
            else
              element[i].subType->printTypeDfltConfig(myvprint("%s%s",element[i].name,ArrS),_subtype,withDescription);
        }
      }
    }
  }
  if (basestr != nullptr) free(basestr);
  else SMILE_PRINT(" ");
}

ConfigType::~ConfigType()
{
  if (element != nullptr) {
    int i;
    for (i=0; i<N; i++) {
      if (element[i].description != nullptr) {
        free(element[i].description);
      }
      if (element[i].dfltStr != nullptr) {
        free(element[i].dfltStr);
      }
      // TODO: delete subType!?
      if ((element[i].freeType)&&(element[i].subType!=nullptr)) delete element[i].subType;
    }
    free(element);
  }
}

// disable field by setting "enabled = 0", the field is still available normally, however, it will not be shown by printTypeHelp anymore!
int ConfigType::disableField(const char *_name)
{
  int FF = findField(_name);
  if (FF > -1) {
    element[FF].enabled = 0;
    return 1;
  }
  return 0;
}

int ConfigType::setField(const char *_name, const char *description, int type, int subtype, const ConfigType *subType, int freeType, int N_, int printDflt)
{
  // check name for forbidden characters: . [ ] = : , ;
  if (strchr(_name,'.') != nullptr) CONF_MANAGER_ERR("ConfigType::setField: Forbidden charachter '.' in field name '%s'",_name);
  if (strchr(_name,',') != nullptr) CONF_MANAGER_ERR("ConfigType::setField: Forbidden charachter ',' in field name '%s'",_name);
  if (strchr(_name,'[') != nullptr) CONF_MANAGER_ERR("ConfigType::setField: Forbidden charachter '[' in field name '%s'",_name);
  if (strchr(_name,']') != nullptr) CONF_MANAGER_ERR("ConfigType::setField: Forbidden charachter ']' in field name '%s'",_name);
  if (strchr(_name,':') != nullptr) CONF_MANAGER_ERR("ConfigType::setField: Forbidden charachter ':' in field name '%s'",_name);
  if (strchr(_name,';') != nullptr) CONF_MANAGER_ERR("ConfigType::setField: Forbidden charachter ';' in field name '%s'",_name);
  if (strchr(_name,'=') != nullptr) CONF_MANAGER_ERR("ConfigType::setField: Forbidden charachter '=' in field name '%s'",_name);

  // check for uniqueness of name:
  int FF = findField(_name);
  if (FF == -1) {
    // check for free space:
    if (I>=N) {
      // DONE: dynamically increase the number of fields
      //CONF_MANAGER_ERR("ConfigType::setField: cannot add more then N=%i fields! Specify a larger N when creating the object!",N);
#define N_NEW_ALLOC 20
      ConfigDescription * tmp = (ConfigDescription *)realloc(element, sizeof(ConfigDescription)*(I+N_NEW_ALLOC));
      if (tmp != nullptr) {
        ConfigDescription * tmp2 = tmp+N;
        // initialise tmp2 with zero
        bzero( tmp2, sizeof(ConfigDescription)*N_NEW_ALLOC);
        N=I+N_NEW_ALLOC;
        element = tmp;
      } else { OUT_OF_MEMORY; }
    }

    element[I].enabled = 1;
    element[I].type = type;
    element[I].printDflt = printDflt;
    element[I].subtype = subtype;
    element[I].subType = subType;
    element[I].freeType = freeType;
#ifdef _MSC_VER // Visual Studio specific macro
    strncpy_s( element[I].name, CONFIGTYPE_STRLEN, _name, CONFIGTYPE_STRLEN );
#else
    strncpy( element[I].name, _name, CONFIGTYPE_STRLEN );
#endif
    //printf("name: %s.%s (d=%s)\n",this->getName(),_name,description); fflush(stdout);
    if (description != nullptr) {
      element[I].description = strdup(description);
    } else { element[I].description = nullptr; }
    //printf("name: %s.%s (Desc)\n",this->getName(),_name); fflush(stdout);
    element[I].N = N_;
    element[I].isMandatory = 0;
    return I++;
  } else {
    element[FF].enabled = 1;
    element[FF].type = type;
    element[FF].printDflt = printDflt;
    element[FF].subtype = subtype;
    //element[FF].subType = subType;  // TODO: subtype overwrite!
    //element[FF].freeType = freeType; // TODO: free on overwrite!?
    //    strncpy( element[FF].name, _name, CONFIGTYPE_STRLEN ); // name may not be changed!
    if (description != nullptr) {
      if (element[FF].description != nullptr) free(element[FF].description);
      element[FF].description = strdup(description);
    }
    element[FF].N = N_;
    //element[FF].isMandatory = 0;
    return FF;
    //  CONF_MANAGER_ERR("ConfigType::setField: '%s' is not a unique name, already used in type!");
  }
  return -1;
}

int ConfigType::setField(const char *_name, const char *description, int dflt, int arr, int printDflt)
{
  int ret=0;
  if (!arr) {
    ret = setField(_name,description,CFTP_NUM,0,(const ConfigType*)nullptr,1,0,printDflt);
  } else {
    ret = setField(_name,description,CFTP_NUM_ARR,0,(const ConfigType*)nullptr,1,0,printDflt);
  }
  if (ret >= 0) element[ret].dfltDouble = (double)dflt;
  return ret;
}

int ConfigType::setField(const char *_name, const char *description, double dflt, int arr, int printDflt)
{
  int ret=0;
  if (!arr) {
    ret = setField(_name,description,CFTP_NUM,0,(const ConfigType*)nullptr,1,0,printDflt);
  } else {
    ret = setField(_name,description,CFTP_NUM_ARR,0,(const ConfigType*)nullptr,1,0,printDflt);
  }
  if (ret >= 0) element[ret].dfltDouble = dflt;
  return ret;
}

int ConfigType::setField(const char *_name, const char *description, const char * dflt, int arr, int printDflt)
{
  int ret=0;
  if (!arr) {
    ret = setField(_name,description,CFTP_STR,0,nullptr,1,0,printDflt);
  } else {
    ret = setField(_name,description,CFTP_STR_ARR,0,nullptr,1,0,printDflt);
  }
  if (ret >= 0) {
    if (dflt != nullptr) {
      if (element[ret].dfltStr != nullptr) free(element[ret].dfltStr);
      element[ret].dfltStr = strdup(dflt);
    } else
      element[ret].dfltStr = nullptr;
  }
  return ret;
}

int ConfigType::setField(const char *_name, const char *description, char dflt, int arr, int printDflt)
{
  int ret=0;
  if (!arr) {
    ret = setField(_name,description,CFTP_CHR,0,nullptr,1,0,printDflt);
  } else {
    ret = setField(_name,description,CFTP_CHR_ARR,0,nullptr,1,0,printDflt);
  }
  if (ret >= 0) element[ret].dfltChar = dflt;
  return ret;
}

int ConfigType::setField(const char *_name, const char *description, const ConfigType *dflt, int arr, int freeType)
{
  int ret=-1;
  if (dflt==nullptr) return ret;
  if (arr==NO_ARRAY) {
    ret = setField(_name,description,CFTP_OBJ,0,dflt,freeType);
  } else {
    ret = setField(_name,description,CFTP_OBJ_ARR,0,dflt,freeType);
  }
  return ret;
}


const ConfigDescription * ConfigType::operator[] (int n)
{
  if ((n<N)&&(n>=0))
    return &(element[n]);
  else CONF_MANAGER_ERR("ConfigType [%i] index out of bounds (0-%i)",n,N);
}

int ConfigType::findField(const char *fname, int *arrI, char **arrS) const
{
  // search for [] array indices:
  if (element == nullptr) return -1;
  if (fname == nullptr) {
    SMILE_DBG(7,"ConfigType::findField: called with fname == nullptr!");
    return -1;
  }
  char *base = strdup(fname);
  char * s = strchr(base,'[');
  int isArr=0;
  if (s!=nullptr) { // [ was found
    isArr=1;
    char * e = strchr(base,']');
    if (e==nullptr) {
      CONF_PARSER_ERR("ConfigType::findField: parse error: field name '%s', missing closing array index markers ] at end of name!",fname);
    }
    if (strlen(e) > 1) {
      CONF_PARSER_ERR("ConfigType::findField: parse error: field name '%s', has array index markers [] not at end of name!",fname);
    }
    s[0] = 0;
    if ((arrI != nullptr)||(arrS != nullptr)) {
      e[0] = 0;
      long Aidx;
      char *idxStr = s+1;
      // TODO: remove spaces from idxStr...
      
      if (strlen(idxStr) <= 0) CONF_PARSER_ERR("ConfigType::findField: parse error: field name '%s', has empty array index markers []!",fname);
      // scan array index...
      char *eptr = nullptr;
      Aidx = strtol(idxStr,&eptr,0);
      if ((eptr != nullptr)&&(eptr[0]!=0)) { // not a pure number!
        if (arrS != nullptr) *arrS = strdup(idxStr);
        if (arrI != nullptr) *arrI = -1;
      } else {
        //sscanf( idxStr, "%i", &Aidx );
        if (arrI != nullptr) *arrI = Aidx;
        if (arrS != nullptr) *arrS = nullptr;
      }
    }
  } else if (arrI != nullptr) { *arrI = -1; }

  // now find "base":
  int i;
  for (i=0; i<N; i++) {
    //if (element[i].name != nullptr) {
      if (!strcmp(element[i].name,base)) {
        free(base);
/*
        if ((element[i].type >= CFTP_ARR)&&(!isArr)) {
          SMILE_ERR(1,"missing array index [] for element '%s'",element[i].name);
          return -1;
        }  TODO: move this somewhere else (in every function that uses findField(H)....!)*/
        if ((element[i].type < CFTP_ARR)&&(isArr)) {
          SMILE_ERR(1,"array index [] specified for non-array element '%s'",element[i].name);
          return -1;
        }
        return i;
      }
    //}
  }
  free(base);
  return -1;  // field not found
}

int ConfigType::getType(int n) const  {
  if ((n>=0)&&(n<N)&&(element!=nullptr)) {
    return element[n].type;
  }
  return -1;
}

const ConfigType * ConfigType::getTypeObj(int n) const {
  if ((n>=0)&&(n<N)&&(element!=nullptr)) {
    return element[n].subType;
  }
  return nullptr;
}

const char * ConfigType::getName(int n) const {
  if ((n>=0)&&(n<N)&&(element!=nullptr)) {
    return element[n].name;
  }
  return nullptr;
}

/**************************************************/


ConfigInstance::ConfigInstance(const char *_name, const ConfigType *_type, int _freeType) :
  type(nullptr),
  freeType(_freeType)
{
  int n;
  if (_name != nullptr) {
#ifdef _MSC_VER // Visual Studio specific macro
	strncpy_s(name,CONFIGTYPE_STRLEN,_name,CONFIGTYPE_STRLEN);
#else
    strncpy(name,_name,CONFIGTYPE_STRLEN);
#endif
    type=_type;
    if (_type != nullptr) {
      n=_type->getN();
      field=(ConfigValue **)calloc(1,sizeof(ConfigValue *)*n);
      if (field == nullptr) OUT_OF_MEMORY;
      N=n;
      int i;
      for (i=0; i<n; i++) {
        switch(_type->getType(i)) {
          case CFTP_NUM : field[i] = new ConfigValueNum(_type->getDfltNum(i)); break;
          case CFTP_STR : field[i] = new ConfigValueStr(_type->getDfltStr(i)); break;
          case CFTP_CHR : field[i] = new ConfigValueChr(_type->getDfltChr(i)); break;
          case CFTP_OBJ : field[i] = new ConfigValueObj(new ConfigInstance(_type->getName(i) ,_type->getDfltObj(i))); break;
          case CFTP_ARR : field[i] = new ConfigValueArr(); break;
          case CFTP_NUM_ARR : field[i] = new ConfigValueNumArr(); break;
          case CFTP_STR_ARR : field[i] = new ConfigValueStrArr(); break;
          case CFTP_CHR_ARR : field[i] = new ConfigValueChrArr(); break;
          case CFTP_OBJ_ARR : field[i] = new ConfigValueObjArr(); break;
          default: CONF_MANAGER_ERR("cannot create field of unknown type constant %i",_type->getType(i));
        }
        if (field[i] != nullptr) field[i]->unset();
      }
    } else {
      CONF_MANAGER_ERR("Canot create a ConfigInstance with _type==nullptr !");
    }
  } else {
    CONF_MANAGER_ERR("Canot create a ConfigInstance with _name==nullptr !");
  }
}

ConfigInstance::~ConfigInstance()
{
  if ((freeType)&&(type!=nullptr)) delete type;
  if (field != nullptr) {
    int i;
    for(i=0; i<N; i++) {
      if (field[i] != nullptr) delete field[i];
    }
    free(field);
  }
}

/* sanity check the given instance, if it is of the same type, dimensions etc. */
int ConfigInstance::sanityCheck(ConfigInstance *_match) const
{
  // do some sanity checks:
  if (_match == nullptr) {
    // argument cannot be nullptr...
    //CONF_MANAGER_ERR("argument of sanityCheck(ConfigInstance *_match) is nullptr!");
    return 0;
  }
  if (_match->field == nullptr) {
    // no values in default
    CONF_MANAGER_ERR("sanityCheck: field array in class passed as parameter is nullptr!");
  }
  if (_match->N != N) { // number of values must match
    SMILE_ERR(2,"cannot update missing values from an Instance with a different number of values");
    return 0;
  }
  if (_match->type->getName() != type->getName()) { // type name must match ??
    SMILE_ERR(3,"type mismatch during update missingFrom");
    return 0;
  }
  return 1;
}

/* returns the number of values that were  taken (updated) from _default */
// TODO: handle array types...
int ConfigInstance::missingFrom(ConfigInstance *_default)
{
  int i,nup=0;
  if (!sanityCheck(_default)) return 0;
  for (i=0; i<N; i++) {
    nup += field[i]->missingFrom(_default->getValue(i));
      //  (field[i]->getType() == CFTP_OBJ)||(field[i]->getType() >= CFTP_ARR)   then hierarchically update...
    //if ((!(field[i]->isSet()))||(field[i]->getType() == CFTP_OBJ)||(field[i]->getType() >= CFTP_ARR)) { // value not set, use _default...
    //  field[i]->copyFrom(_default->getValue(i));
    //  nup++;
    //}
  }
  return nup;
}

/* returns number of values overwritten (=number of set values in _new) */
int ConfigInstance::updateWith(ConfigInstance *_new)
{
  int i,nov=0;
  if (!sanityCheck(_new)) return 0;
  for (i=0; i<N; i++) {
    field[i]->updateWith(_new->getValue(i));
   // if (!(_new->field[i]->isSet())) { // value not set, use _default...
   //   field[i]->copyFrom(_new->getValue(i));
   //   nov++;
   // }
  }
  return nov;
}


/*
   find field (value) in instance by field name (hierarchical)
   return 1 on success, 0 on error
   
   optional: set *in to address of ConfigInstance containig the found value/field
                 and *n to index of field in that instance.
                 (only valid, if not nullptr is returned)
*/
int ConfigInstance::findField(const char *_name, int *n, ConfigInstance **in, int *aIdx, char **aStr)
{
  try {
  if (_name != nullptr) {
    int h, idx;
    char *base=nullptr;
    ConfigInstance *cur=this;
    const char*origname = _name;
    int arrIdx = -1;
    char *arrStr = nullptr;
    
    do {
      if (cur==nullptr) {
        CONF_MANAGER_ERR("ConfigInstance::findField: cannot dereference nullptr subobject pointer while finding value for '%s' (rem:'%s')",origname,_name);
      }
      // split name
      const char *rem;
      h = instNameSplit(_name, &base, &rem);
      arrIdx = -1;
      if (arrStr != nullptr) { free(arrStr); arrStr = nullptr; }
      idx = cur->type->findField(base,&arrIdx,&arrStr);
      if (base!=nullptr) { free(base); base=nullptr; }
      _name=rem;

      if (h==1) {
        if ((idx >= 0)&&(idx < N)) { // check for valid index
          if ((cur->field[idx]->getType() == CFTP_OBJ)||(cur->field[idx]->getType() == CFTP_OBJ_ARR)) {
            // TODO... check if field is initialized ???!!!!!
            if (field[idx] == nullptr) CONF_MANAGER_ERR("ConfigInstance::findField: attempting to access object field '%s', which has not been initialized (full name: '%s')!",base,origname);
            cur = cur->field[idx]->getObj(arrIdx);
          }
          else
            CONF_MANAGER_ERR("ConfigInstance::findField: subtype object referenced in '%s', however field %i is not of type OBJ or OBJ_ARR!",_name,idx);
        } else {
          // error field "base" not found...
          CONF_MANAGER_ERR("ConfigInstance::findField: referenced base field with name '%s' not found!",origname);
        }
      }
    } while (h==1);

    if (h==0) {
      if ((idx >= 0)&&(idx < N)) { // check for valid index
        if (n!=nullptr) *n = idx;
        if (in!=nullptr) *in = cur;
        if (aIdx!=nullptr) *aIdx = arrIdx;
        if (aStr!=nullptr) *aStr = arrStr;
        return 1;
      } else
        if (arrStr != nullptr) free(arrStr);
        // error field "base" not found...
        CONF_MANAGER_ERR("ConfigInstance::findField: referenced base field with name '%s' not found!",origname);
    }
    if (arrStr != nullptr) free(arrStr);
  }
  } catch (ConfigException *) {}
  return 0;
}

int ConfigInstance::getType(const char *_name)
{ /* recursively dereference, via findField */
  int n,aIdx;
  ConfigInstance *child=nullptr;
  int r = findField(_name, &n, &child, &aIdx, nullptr);
  if ((r)&&(child!=nullptr)) {
    return child->getType(n);
  } else { return r; }
}

/* this function does not check n for valid range!!*/
/* the memory pointed to by val is freed, if the content is copied over exisiting content,
   otherwise the memory is not freed and the pointer is copied. the memory pointed to by
   val may NEVER be freed by the calling code, however it must be allocated by the calling code */
void ConfigInstance::setVal(int n, ConfigValue *val, int idx)
{
  if (val == nullptr) return;
  if (field[n] == nullptr) {
    int ty = type->getType(n);
    if (ty != val->getType()) CONF_MANAGER_ERR("ConfigInstance::setVal: Type mistmatch *val : %i != getType(%i) : %i",val->getType(),ty);
    if ((ty >= CFTP_ARR)&&(idx>=0)) {
      // a) array -> create array value for type
      switch(ty) {
        case CFTP_NUM_ARR: field[n] = new ConfigValueNumArr(idx+1); break;
        case CFTP_STR_ARR: field[n] = new ConfigValueStrArr(idx+1); break;
        case CFTP_CHR_ARR: field[n] = new ConfigValueChrArr(idx+1); break;
        case CFTP_OBJ_ARR: field[n] = new ConfigValueObjArr(idx+1); break;
        default: CONF_MANAGER_ERR("unknonwn array type %i for field idx=%i encountered in ConfigInstance::setVal!",ty,n);
      }
      if (field[n] == nullptr) OUT_OF_MEMORY;
      field[n]->setValue(val,idx); // this is bogus , TODO: copyFrom for array element!
    } else {
      // b) non-array, or full array passed as *val parameter (when idx==-1): copy val
      field[n] = val;
    }
  } else { // copy values
    if ((idx >= 0)&&(field[n]->getType() >= CFTP_ARR)) {
      // a) array -> use copyFrom with array index...
      ConfigValue *tmp = (*(ConfigValueArr*)(field[n]))[idx];
      if ( tmp != nullptr) {
        tmp->copyFrom(val);     delete val;
      } else
        field[n]->setValue(val,idx);
    } else {
      // b) non-array:
      field[n]->copyFrom(val);  // check type??
      delete val;
    }
  }
}

const ConfigValue * ConfigInstance::getVal(int n, int idx) const {
  if ((idx >= 0)&&(field[n]->getType() >= CFTP_ARR)) {
    // a) array -> get element [idx]
    return (*(ConfigValueArr*)(field[n]))[idx];
  } else {
    // b) non-array or full array element:
    return field[n];  
  }
}

/* recursively dereference subobj pointers */
/* does not copy *val, it stores the pointer directly in the configInstance object */
void ConfigInstance::setValue(ConfigValue *val, int n, const char *_name, int arrIdx)
{
  if (n >= 0) {  // prefer N
    if (n<N) {
      setVal(n,val,arrIdx);
    }
  } else if (_name != nullptr) {
    SMILE_DBG(7,"called ConfigInstance::setValue (_name = '%s')",_name);
    // split name
    const char *rem=nullptr;
    char *base=nullptr;
    int h = instNameSplit(_name, &base, &rem);
    arrIdx=-1;
    char *arrStr=nullptr;
    int idx = type->findField(base,&arrIdx,&arrStr);
    SMILE_DBG(7,"findField returned idx=%i and arrIdx=%i  arrStr='%s' (h=%i)",idx,arrIdx,arrStr,h);
    if (h==1) {
      if ((idx >= 0)&&(idx < N)) { // check for valid index
        if (field[idx] == nullptr) CONF_MANAGER_ERR("ConfigInstance::getValue: attempting to access object field '%s', which has not been initialized!",base);
        if ((field[idx]->getType() == CFTP_OBJ)||(field[idx]->getType() == CFTP_OBJ_ARR)) {
          // TODO: if field[idx] not allocated... create a new Instance...!
          if (field[idx] == nullptr) { // ... should never happen....
             SMILE_DBG(7,"ConfigInstance::setValue : creating new field (base = '%s') ... STRANGE!?",base);
             field[idx] = new ConfigValueObj(new ConfigInstance(base, type->getTypeObj(idx) ));
          }
          if (arrStr != nullptr) {
            arrIdx = field[idx]->findFieldWr(arrStr);
            free(arrStr);
          }
          SMILE_DBG(7,"ConfigInstance::setValue : arrIdx = %i, isset = %i",arrIdx, field[idx]->isSet(arrIdx));
          
          if (!(field[idx]->isSet(arrIdx))) {
            SMILE_DBG(7,"ConfigInstance::setValue : creating new array element (base = '%s') ai:%i",base,arrIdx);
            field[idx]->setValue(new ConfigInstance(base, type->getTypeObj(idx) ),arrIdx);
          }
          ConfigInstance *cur = field[idx]->getObj(arrIdx);
          if (cur!=nullptr) cur->setValue(val,-1,rem);
          else
            CONF_MANAGER_ERR("setValue: cannot dereference nullptr subobject pointer while setting value for '%s' (rem:'%s')",_name,rem);
        }
        else
          CONF_MANAGER_ERR("subtype object referenced in '%s', however field %i is not of type OBJ or OBJ_ARR!",_name,idx);
      } else CONF_MANAGER_ERR("setValue: hierarchical field referenced ('%s') does not exist! ",rem);
    } else {
      if (arrStr != nullptr) {
        arrIdx = field[idx]->findFieldWr(arrStr);
        free(arrStr);
      }
      setValue(val,idx,nullptr,arrIdx);
    }
    if (base!=nullptr) { free(base); base=nullptr; }
  }
}

const ConfigValue * ConfigInstance::getValue(int n, const char *_name, int arrIdx) const
{
  if (n >= 0) {  // prefer N
    if (n<N) {
      return getVal(n,arrIdx);
    }
  } else if (_name != nullptr) {
    // split name
    SMILE_DBG(7,"called ConfigInstance::getValue (_name = '%s')",_name);
    const char *rem;
    char *base=nullptr;
    int h = instNameSplit(_name, &base, &rem);
    arrIdx = -1;
    char *arrStr = nullptr;
    int idx = type->findField(base,&arrIdx,&arrStr);
    SMILE_DBG(7,"findField returned idx=%i and arrIdx=%i arrStr='%s' (h=%i)",idx,arrIdx,arrStr,h);
    if (base!=nullptr) { free(base); base=nullptr; }
    if (h==1) {
      if ((idx >= 0)&&(idx < N)) { // check for valid index
        if (field[idx] == nullptr) CONF_MANAGER_ERR("ConfigInstance::getValue: attempting to access object field '%s', which has not been initialized!",base);
        if ((field[idx]->getType() == CFTP_OBJ)||(field[idx]->getType() == CFTP_OBJ_ARR)) {
          // TODO:: check if field[idx] is initialized!!
          if (arrStr != nullptr) {
            arrIdx = field[idx]->findField(arrStr);
            free(arrStr);
          }
          ConfigInstance *cur = field[idx]->getObj(arrIdx);
          if (cur!=nullptr) return cur->getValue(-1,rem);
          else
            CONF_MANAGER_ERR("getValue: cannot dereference nullptr subobject pointer while getting value for'%s' (rem:'%s')",name,rem);
        } else CONF_MANAGER_ERR("getValue: hierarchical field referenced is not of type object! ('%s')",rem);
      } else CONF_MANAGER_ERR("getValue: hierarchical field referenced ('%s') does not exist! ",rem);
    } else {
      if (idx < 0) SMILE_WRN(2,"ConfigInstance::getValue : non-existant field '%s' (%i)",_name,n);
      if (arrStr != nullptr) {
        arrIdx = field[idx]->findField(arrStr);
        free(arrStr);
      }
      return getValue(idx,nullptr,arrIdx);
    }
  }
  SMILE_WRN(2,"ConfigInstance::getValue : index %i out of bounds (0-%i)",n,N-1);
  return nullptr;
}



/*******************************************************************************/
/* generic backend reader interface, virtual class only, descendant must implement:
     findInstancesByName
     getInstance
     openInput
            */

cConfigReader::cConfigReader(const char *myInputPath, int myInputId, cCommandlineParser *myCmdparser) :
  inputPath(nullptr),
  lastLevelFile(nullptr),
  inputId(myInputId),
  cmdparser(myCmdparser)
{
  SMILE_DBG(4, "creating a new cConfigReader component");
  if (myInputPath != nullptr) 
    inputPath = strdup(myInputPath);
  // derived class must call openInput()!?
}

void cConfigReader::destroyStrArray(char **arr, int n)
{
  int i;
  if (arr != nullptr) {
    for (i=0; i<n; i++) {
      if (arr[n] != nullptr) free(arr[n]);
    }
    free(arr);
  }
}

char ** cConfigReader::findInstances(const ConfigType *_type, int *N)
{
  if (_type != nullptr) {
    return findInstancesByTypeName(_type->getName(),N);
  }
  return nullptr;
}

cConfigReader::~cConfigReader()
{
  //closeInput();  must be called by derived class... ?
  if (inputPath != nullptr) 
    free(inputPath);
  if (lastLevelFile != nullptr) 
    free(lastLevelFile);
}

/*******************************************************************************/
/* commandline config reader */
#if 0
class cCommandlineConfigReader : public cConfigReader {
  protected:
    char **argv;
    int argc;

  public:
    cCommandlineConfigReader(const char **_argv, int _argc);
    virtual int openInput() {} // nothing to open here...
    virtual int closeInput() {}

    virtual char ** findInstancesByTypeName(const char *_typename, int *N);  /* returns names (and no.) of instances of type "typename" */
    virtual ConfigInstance *getInstance(const char *_instname, const ConfigType *_type);    /* get an instance with name _instname */
    virtual ~cConfigReader() {}
};

cCommandlineConfigReader::cCommandlineConfigReader(const char **_argv, int _argc)
{

}
#endif

/*******************************************************************************/
/* ini-style file config reader */

/* file format:

[InstanceName:TypeName]
InstanceName.Field1.subfield=value
InstanceName.Field1.myarray[0]=value
InstanceName.Field1.myarray[1]=value
InstanceName.NextField = value
[NextInstanceName:TypeName]

*/

int cFileConfigReader::addInst(const char*_instname, const char*_typename)
{
  // search for existing instances, warn but add to it...
  int i;
  for (i=0; i<nInst; i++) {
    if ( (inst[i].name != nullptr) && (_instname != nullptr) && (!strcmp(inst[i].name,_instname)) ) {
      // instance with same name found
      if ((inst[i].type != nullptr)&&(_typename != nullptr)) {
        if (!strcmp(inst[i].type,_typename)) { // type also matches
          SMILE_WRN(4, "cFileConfigReader::addInst:  duplicate instance '%s' in config file (type='%s'), these instances will be merged to one.", _instname, _typename);
          return i; // append to this type...
        } else { // type mismatch: ERR!
	  CONF_MANAGER_ERR("duplicate instance '%s' in config file has conflicting types '%s'<->'%s' (duplicate instances must be of the same type, the content will be appended)\n",_instname,inst[i].type,_typename);
        }
      } else {
        if (_typename != inst[i].type) { 
          CONF_MANAGER_ERR("duplicate instance '%s' in config file has conflicting types '%s'<->'%s' (duplicate instances must be of the same type, the content will be appended)\n",_instname,inst[i].type,_typename);
        }
      }
    }
  }

  if (nInst >= nInstAlloc) { // realloc
    fileInstance *f = (fileInstance *)realloc( inst, sizeof(fileInstance) * (nInst+10));
    if (f == nullptr) OUT_OF_MEMORY;
    inst = f;
    nInstAlloc = nInst+10;
  }
  inst[nInst].name = strdup(_instname);
  inst[nInst].type = strdup(_typename);
  SMILE_DBG(6,"cFileConfigReader , added instance: '[%s:%s]'",inst[nInst].name,inst[nInst].type);
  inst[nInst].N=0;
  inst[nInst].Nalloc=0;
  inst[nInst].lines = nullptr;
  inst[nInst].lineNr = nullptr;
  return nInst++;
}

int cFileConfigReader::addLine(int n, const char *line, int lineNr)
{
  if ((n<0)||(n>=nInst)) return -1;
  if (inst[n].N >= inst[n].Nalloc) { // realloc
    char **l = (char **)realloc(inst[n].lines, sizeof(char*) * (inst[n].N+100) );
    if (l==nullptr) OUT_OF_MEMORY;
    inst[n].lines = l;
    inst[n].Nalloc = inst[n].N+100;
    int *ln = (int *)realloc(inst[n].lineNr, sizeof(int) * (inst[n].N+100) );
    if (ln==nullptr) OUT_OF_MEMORY;
    inst[n].lineNr = ln;
  }
  inst[n].lines[inst[n].N] = strdup(line);
  inst[n].lineNr[inst[n].N] = lineNr;
  return inst[n].N++;
}

// TODO: cache config files which have already been loaded once (including all includes?)
/* each instance has a header: [instname:type] followed by the lines with values */
/* including other config files can be done via the command:  \{path/and/file_to.include} */
int cFileConfigReader::openInput(const char*fname, int *idx0)
{
  FILE *in=nullptr;
  char *localThisLevelFile = nullptr;
  // open file "_inputpath"
  if (fname == nullptr) {
    SMILE_MSG(3, "reading config file '%s'", inputPath);
    in = fopen(inputPath, "r");
    if (in == nullptr) 
      CONF_MANAGER_ERR("cFileConfigReader::openInput : cannot open input file '%s'!",inputPath);
    if (lastLevelFile != nullptr)
      free(lastLevelFile);
    lastLevelFile = nullptr;
  } else {
    SMILE_MSG(3,"reading config file '%s'",fname);
    if (!strcmp(fname,inputPath)) CONF_MANAGER_ERR("loop in config file includes detected (the base file '%s' was included in itself)!",inputPath);
    // try absolute path first...
    in = fopen( fname, "r" );
    if (in == nullptr) {
      // try relative to inputPath next
      char * fname2 = strdup(inputPath);
      char * pt = strrchr(fname2,'/');
      if (pt == nullptr) pt = strrchr(fname2,'\\');
      if ((pt != nullptr)&&(pt != fname)) {
        *pt = 0;
      } 
      char * fname3 = myvprint("%s/%s",fname2,fname);
      if (fname3 != nullptr) {
        SMILE_MSG(3,"config file '%s' not found, trying to open '%s'.",fname,fname3);
        in = fopen(fname3, "r");
        if (in == nullptr) {
          // try relative to path of previously included file (if not same as inputPath)
          if (lastLevelFile != nullptr && lastLevelFile != inputPath) {
            char * fname2b = strdup(lastLevelFile);
            char * ptb = strrchr(fname2b,'/');
            if (ptb == nullptr) ptb = strrchr(fname2b,'\\');
            if ((ptb != nullptr)&&(ptb != fname)) {
              *ptb = 0;
            } 
            char * fname3b = myvprint("%s/%s", fname2b, fname);
            if (fname3b != nullptr) {
              SMILE_MSG(3,"config file '%s' not found, trying to open '%s'.",fname3,fname3b);
              in = fopen(fname3b, "r");
              if (in == nullptr) {
                CONF_MANAGER_ERR("cFileConfigReader::openInput : cannot open input file '%s'. Fatal!", fname3b);
              } else {
                if (lastLevelFile != nullptr)
                  free(lastLevelFile);
                lastLevelFile = strdup(fname3b);
              }
              free(fname3b);
            } else {
              CONF_MANAGER_ERR("cFileConfigReader::openInput : cannot open input file '%s'. Fatal!", fname3);
            }
            free(fname2b);
          } else {
            CONF_MANAGER_ERR("cFileConfigReader::openInput : cannot open input file '%s'. Fatal!", fname3);
          }
        } else {
          if (lastLevelFile != nullptr)
            free(lastLevelFile);
          lastLevelFile = strdup(fname3);
        }
        free(fname3);
      } else {
        CONF_MANAGER_ERR("cFileConfigReader::openInput : cannot open input file '%s'. Fatal!",fname);
      }
      free(fname2);
    } else {
      if (lastLevelFile != nullptr)
        free(lastLevelFile);
      lastLevelFile = strdup(fname);
    }
  }
  
  if (lastLevelFile != nullptr) {
    localThisLevelFile = strdup(lastLevelFile);
  }

  // read file, 1st pass to find Types & Names
  char *line=nullptr;
  int nline=-1;
  int idx = -1;
  if (idx0!=nullptr) idx=*idx0;
  int lineNr = 0;
  size_t n,read;
  int inComment = 0;

  do {
    read = smile_getline(&line, &n, in);
    char *origline = line;
    
    if ((read != (size_t)-1)&&(origline!=nullptr)) { ///XXXX
      lineNr++;
      // remove newline at end:
      int len = (int)strlen(line)-1;
      if (len>=0) { if (line[len] == '\n') { line[len] = 0; len--; } }
      if (len>=0) { if (line[len] == '\r') { line[len] = 0; len--; } }
      while (((line[0] == ' ')||(line[0] == '\t'))&&(len>=0)) { line[0] = 0; line++; len--; }

      if (len >= 0) {

    // NEW: multi-line comments , c-style: /* */
    if ( ((len>0)&&(line[0]=='/')&&(line[1]=='*')) )
      {  inComment = 1; } 

    if (inComment) {
      if ( ((len>0)&&( ((line[0]=='*')&&(line[1]=='/'))||((line[len]=='/')&&(line[len-1]=='*')) ) ) )
        {  inComment = 0; } 
      if (origline != nullptr) { free(origline); line = nullptr; }
      continue; // skip commented-out lines
    }


      SMILE_DBG(8,"read line: '%s' read=%i, len=%i",line,read,len);
      if (line[0]=='[') { // check for instance header
        char *ty = strchr(line,':');
        if (ty==nullptr) { CONF_PARSER_ERR("(line %i) error parsing '%s':\n %s missing ':' separating instance name and type!",lineNr,inputPath,line); }
        ty[0] = 0; ty++;
        char *tmp = strchr(ty,':');
        if (tmp!=nullptr) { CONF_PARSER_ERR("(line %i) error parsing '%s':\n ':' cannot appear in config type name!",lineNr,inputPath,line); }
        tmp = strchr(ty,']');
        if (tmp==nullptr) { CONF_PARSER_ERR("(line %i) error parsing '%s':\n %s missing ']' after instance type!",lineNr,inputPath,line); }
        tmp[0] = 0;
        char *ins = line+1;
        idx = addInst(ins,ty);
        //free(line); line = nullptr;
      } else if ((len>4)&&(line[0] == '\\')&&(line[1] == '{')&&(line[len]=='}')) { // include config file
        line[len]=0;
        const char*fn = line+2;
/*TODO: support \cm here until a unified interface in openInput has been designed.*/
        if (fn[0] == '\\' && fn[1] == 'c' && fn[2] == 'm' && fn[3] == '[' && len > 8) {  //....
          char *value = strdup(fn);
          if (cmdparser != nullptr && value != nullptr) {
            char *_long = value+4;
            char *_short = strchr(value,'(');
            char *_dflt = strchr(value,'{');
            char *_descr = strchr(value,':');
            if (_short!=nullptr) {
              *_short = 0;
              _short++;
              char *_tmp = strchr(_short,')');
              if (_tmp == nullptr) { CONF_MANAGER_ERR("line %i : missing ) in commandline reference '%s'",lineNr,value); }
              else *_tmp = 0;
              if (strlen(_short) > 1) { CONF_MANAGER_ERR("line %i : short option () in commandline reference '%s' is longer than one character!",lineNr,value); }
            }
            if (_dflt!=nullptr) {
              *_dflt = 0;
              _dflt++;
              char *_tmp = strchr(_dflt,'}');
              if (_tmp == nullptr) { CONF_MANAGER_ERR("line %i : missing } in commandline reference '%s'",lineNr,value); }
              else *_tmp = 0;
              if (strlen(_dflt) < 0) { CONF_MANAGER_ERR("line %i : dflt value {} in commandline reference '%s' is too short!",lineNr,value); }
            }
            if (_descr!=nullptr) {
              *_descr = 0;
              _descr++;
              char *_tmp = strchr(_descr,']');
              if (_tmp == nullptr) { CONF_MANAGER_ERR("line %i : missing ] at end of commandline reference '%s'",lineNr,value); }
              else *_tmp = 0;
              if (strlen(_descr) < 0) { CONF_MANAGER_ERR("line %i : description :..] in commandline reference '%s' is too short!",lineNr,value); }
            }
            char *_tmp = strchr(_long,']');
            if (_tmp != nullptr) { *_tmp = 0; }
            char __s=0;
            if (_short!=nullptr) __s = *_short;
            
            if ( (_dflt==nullptr)&&(_descr==nullptr) ) { // old option
              // do nothing...
            } else { // new option
              if (!cmdparser->optionExists(_long)) {  // option does not yet exist
                cmdparser->addStr(_long, __s, _descr, _dflt);
                // Re-parsing must be enabled here, even if it's inefficient. Otherwise we are not able to read the newly created commandline options into the config.
                cmdparser->doParse(1);  // ignore duplicates...
              }
            }
            
            const char *_tmps = cmdparser->getStr(_long);
            char * tmpstr = nullptr;
            if (_tmps == nullptr) {
              SMILE_ERR(1,"configManager: commandline option '%s' has nullptr as default value, please check the \\cm options in the config file (at least one for each option has to have a default value or description given!)",_long);
              tmpstr = strdup("");
            } else {
              tmpstr = strdup(_tmps);
            }
            free(value);
            value = tmpstr;
          } else {
            CONF_MANAGER_ERR("commandline reference specified in line %i, however no commandline parser is present!",lineNr);
          }
          if (value != nullptr) {
            openInput(value,&idx);
            free(value);
            // restore last level file:
            if (lastLevelFile != nullptr) {
              free(lastLevelFile);
              lastLevelFile = nullptr;
            }
            if (localThisLevelFile != nullptr) {
              lastLevelFile = strdup(localThisLevelFile);
            }
          }
        } else {
          openInput(fn,&idx);
          // restore last level file:
          if (lastLevelFile != nullptr) {
            free(lastLevelFile);
            lastLevelFile = nullptr;
          }
          if (localThisLevelFile != nullptr) {
            lastLevelFile = strdup(localThisLevelFile);
          }
        }
      } else {
        addLine(idx,line,lineNr);
      }
      }
    }
    if (origline != nullptr) { 
      free(origline); 
      line = nullptr; 
    }
  } while (read != (size_t)(-1));
  fclose(in);
  if (line != nullptr) { 
    free(line); 
    line = nullptr; 
  }
  if (fname == nullptr) {
    SMILE_MSG(3,"successfully read config file '%s'", inputPath);
  } else {
    SMILE_MSG(3,"successfully read config file '%s'", localThisLevelFile);
  }
  if (localThisLevelFile != nullptr)
    free(localThisLevelFile);
  if (idx0 != nullptr)
    *idx0 = idx;
  return 1;
}


cFileConfigReader::~cFileConfigReader()
{
  int i;
  if (inst != nullptr) {
    for (i=0; i<nInst; i++) {
      if (inst[i].name != nullptr) free(inst[i].name);
      if (inst[i].type != nullptr) free(inst[i].type);
      if (inst[i].lineNr != nullptr) free(inst[i].lineNr);
      if (inst[i].lines != nullptr) {
        int j;
        for (j=0; j<inst[i].N; j++) {
          if (inst[i].lines[j] != nullptr) free(inst[i].lines[j]);
        }
        free(inst[i].lines);
      }
    }
    free(inst);
  }
}

char ** cFileConfigReader::findInstancesByTypeName(const char *_typename, int *N)  /* returns names (and no.) of instances of type "typename" */
{
  int i;
  char **insts=nullptr;
  int n=0;
  if (_typename == nullptr) return nullptr;
  if (N == nullptr) return nullptr;

  SMILE_DBG(7,"cFileConfigReader::findInstancesByTypeName: typename=%s",_typename);
  for (i=0; i<nInst; i++) {
    if (!strcmp(inst[i].type,_typename)) n++;
  }
  *N = n;
  insts = (char **) calloc(1,sizeof(char*)*n);
  n=0;
  for (i=0; i<nInst; i++) {
    if (!strcmp(inst[i].type,_typename)) {
      insts[n++] = strdup(inst[i].name);
      SMILE_DBG(7,"found inst : '%s'",inst[i].name);
    }
  }
  return insts;
}

/*
format of a config line:
 name.name[nr].name.name{str}.name = val;val;val

 comments: line begins with ; or # or //
 name may not contain: [ ] . : = ; , \

 val can be either numeric, string, or config instance reference:  0.00   blah   \in[instname]
   or commandline option reference: \cm[long(short){dflt}:description]   (to add a new option)
      or cmdline reference : \cm[long]     (to reference an existing option)
   or value of another field      : $fieldname
 val may not contain ; or $ or \ as first character
 TODO later: add \; and \$ and \\ for quoting
 
 TODO??? array by  ;  ..!

*/
/* parse lines of an instance...*/
ConfigInstance *cFileConfigReader::getInstance(const char *_instname, const ConfigType *_type, cConfigManager *cman)    /* get an instance with name _instname */
{
  ConfigInstance *ret = new ConfigInstance( _instname, _type, 0 );
  if (ret == nullptr) OUT_OF_MEMORY;
  
  int i,idx;
  // find instance index
  SMILE_DBG(7,"cFileConfigReader::getInstance: instname=%s",_instname);
  for (idx=0; idx<nInst; idx++) {
    if (!strcmp(inst[idx].name,_instname)) break;
  }
  SMILE_DBG(7,"cFileConfigReader::getInstance: instname=%s -> idx=%i",_instname,idx);
  if (idx >= nInst) CONF_PARSER_ERR("cFileConfigReader::getInstance: requested instance name '%s' not found in config file!",_instname);
  //now go through instance and parse lines
  for (i=0; i<inst[idx].N; i++) {
    char *line = strdup(inst[idx].lines[i]);
    int lineNr = inst[idx].lineNr[i];
    char *origline = line;  // we use this later in the call to free();
    if (line==nullptr) OUT_OF_MEMORY;
    SMILE_DBG(7,"cFileConfigReader::getInstance: parsing line no. %i\n     %s",lineNr,line);

    // remove CR(+LF) at end
    int l = (int)strlen(line)-1;
    if (line[l] == '\n') { line[l] = 0; l--; }
    if (line[l] == '\r') { line[l] = 0; l--; }
    
    //remove spaces and tabs at end and beginning
    while (((line[l] == ' ')||(line[l] == '\t'))&&(l>=0)) { line[l] = 0; l--; }
    while (((line[0] == ' ')||(line[0] == '\t'))&&(l>=0)) { line[0] = 0; line++; l--; }

    if (l<0) { if (origline != nullptr) free(origline); continue; } // skip empty lines
    
    // check for comments:
    if ((line[0] == '%')||(line[0] == '#')||(line[0]==';')||((l>0)&&(line[0]=='/')&&(line[1]=='/')))
      { if (origline != nullptr) free(origline); continue; } // skip commented-out lines
    
    // NEW: EOL comments!!  ONLY with '//'
    if (l>1) {
      char *cc = strstr(line,"//");
      if (cc != nullptr) {
        *cc = 0; // remove commented out rest of this line...
      }
    }

      // TODO: search for \in[]  -> link to instance
      // return nullptr and name of instance to link to

    // split at FIRST =
    char *field=line;
    char *value;
    value=strchr(line,'=');
    if ( value == nullptr ) CONF_PARSER_ERR("(line %i) cFileConfigReader::getInstance: missing '=' in line %i '%s' of instance '%s'",lineNr,i,line,inst[idx].name);
    value[0]=0;
    value++;

    int arrT=0,arrN=-1; char *arrS=nullptr;

    // remove basename (instance name)
    field = strchr(line,'.');
    if (field == nullptr) field = line;//CONF_PARSER_ERR("cFileConfigReader::getInstance: missing field name in line '%s'",line);
    else {
      field[0] = 0; 
      // check if first part of line really was _instname..
      if (strcmp(line,_instname)) {
        field[0] = '.';
        field = line;
      } else { field++; }
    }

    l=(int)strlen(field)-1;
    // remove whitespaces at end of fieldname
    while (((field[l] == ' ')||(field[l] == '\t'))&&(l>0)) { field[l] = 0; l--; }

    // get field basename:
/*
    char *fieldbn = nullptr;
    char *nex = strchr(field,'.');
    if (nex != nullptr) {
      nex[0] = 0;
      fieldbn = strdup(field);
      nex[0] = '.';
    } else {
      fieldbn = strdup(field);
    }
*/

    // check for array basename AND array type of field
    if (l>=0) {
      SMILE_DBG(8,"       field=%s",field);
//printf("       field=%s\n",field); fflush(stdout);
      if (field[l] == ']') { // numeric or associative array
        field[l] = 0;
        char * v = strrchr(field,'[');
        if (v == nullptr) CONF_PARSER_ERR("(line %i) cFileConfigReader::getInstance: execess ] found at end of field name!",lineNr);
        v++;
        // associative array [] support : DONE
        
//        int val=0;
        char *eptr=nullptr;
        long rn = strtol(v,&eptr,0);
        if (strlen(v) <= 0) CONF_PARSER_ERR("(line %i) cFileConfigReader::getInstance: nothing specified in [] as array index!",lineNr);
        if ((eptr!=nullptr)&&(eptr[0]!=0)) { // invalid characters -> no number!
          arrT=2; arrS = strdup(v);
        } else {
//        int rn = sscanf(v,"%i",&val);
//        if (rn!=1) CONF_PARSER_ERR("(line %i) cFileConfigReader::getInstance: no number specified in [] as array index!");
          arrT=1; //arrN = val;
          arrN=rn;  // XXXX
        }
        field[l] = ']';
      }
/*
      else if (field[l] == '}') { // associative string array
        field[l] = 0;
        char * v = strrchr(field,'{');
        if (v == nullptr) CONF_PARSER_ERR("(line %i) cFileConfigReader::getInstance: execess } found at end of field name!");
        v++;
        int val;
        int rn = strlen(v);
        if (v<=0) CONF_PARSER_ERR("(line %i) cFileConfigReader::getInstance: no name specified in {} as array index!");
        arrT=2; arrS = strdup(v);
        field[l] = '}';
      }
*/
    } else {
      CONF_PARSER_ERR("(line %i) cFileConfigReader::getInstance: invalid field name '%s'",lineNr,field);
    }

    // check for presence of unquoted ;  => initialize array values of numeric index array, if array type matches )
    char *a = strchr(value,';');
    if ((a>value)&&(*(a-1)!='\\')&&(a != nullptr)) { // this should enable quoting of ; via \ ... ???
      arrT=10;arrN=-1;
//printf("arrT10\n"); fflush(stdout);
    }

    if (arrS != nullptr) free(arrS);
    
//    if (arrT==10) {
      // TODO: split values, convert each value, and save in array element
//      SMILE_ERR(1,"(line %i) arrT==10 not yet implemented! (array elements split by ';')",lineNr);

//    } else
    {
/*   optional: set *in to address of ConfigInstance containig the found value/field
                 and *n to index of field in that instance.
                 (only valid, if not nullptr is returned)
*/
      int n,aIdx;
      const ConfigType *tp=nullptr;

      int h = _type->findFieldH(field, &n, &tp, &aIdx, nullptr);

      int ty;
      if (tp != nullptr) {
        ty = tp->getType(n);
      } else {
        // TODO: Error!!!
        SMILE_ERR(4,"type not found!");
        ty=-1;
      }
      SMILE_DBG(8,"type = %i",ty);
      if (ty>=CFTP_ARR) {
        if (arrT == 0) //CONF_PARSER_ERR("(line %i) expected array index for array type",lineNr,line);
          {
            SMILE_WRN(5,"(line %i) array type field with only one element and no array index in [], assuming arrT=10 and N=1",lineNr);
            arrT=10;
          }
        ty-=(CFTP_ARR+1);
      }

//--
/*
TODO: move this parsing to the openInput function allowing for more generic replacements..?
*/

    l = (int)strlen(value);
    //remove spaces at end and beginning
    while (((value[l] == ' ')||(value[l] == '\t'))&&(l>0)) { value[l] = 0; l--; }
    while (((value[0] == ' ')||(value[0] == '\t'))&&(l>0)) { value[0] = 0; value++; l--; }
    if (l==0) CONF_PARSER_ERR("(line %i) cFileConfigReader::getInstance: empty value in line %i '%s' of instance '%s'",lineNr,i,line,inst[idx].name);
    SMILE_DBG(8,"       value=%s",value);


      // resolve commandline references RIGHT HERE:: :-)
      char *tmpstr=nullptr;
        /* commandline option reference: \cm[long(short){dflt}:description]  */
      int vl=(int)strlen(value);
      if (vl>=5) {
        char *endOfOption = strchr(value, ']');
        if ((value[0]=='\\')&&(value[1]=='c')&&(value[2]=='m')&&(value[3]=='[')&&(endOfOption > value + 5)) { // &&(value[vl-1]==']')) { // \cm[*]
          if (cmdparser != nullptr) {
            if (endOfOption == nullptr) {
              CONF_MANAGER_ERR("line %i: missing ']' in commandline option definition!", lineNr);
            }
            char *_long = value+4;
            char *_short = strchr(value,'(');
            char *_dflt = strchr(value,'{');
            char *_descr = strchr(value,':');
            if (_short > endOfOption)
              _short = nullptr;
            if (_dflt > endOfOption)
              _dflt = nullptr;
            if (_descr > endOfOption)
              _descr = nullptr;
            if (_short!=nullptr) {
              *_short = 0;
              _short++;
              char *_tmp = strchr(_short,')');
              if (_tmp == nullptr) { CONF_MANAGER_ERR("line %i : missing ) in commandline reference '%s'",lineNr,value); }
              else *_tmp = 0;
              if (strlen(_short) > 1) { CONF_MANAGER_ERR("line %i : short option () in commandline reference '%s' is longer than one character!",lineNr,value); }
            }
            if (_dflt!=nullptr) {
              *_dflt = 0;
              _dflt++;
              char *_tmp = strchr(_dflt,'}');
              if (_tmp == nullptr) { CONF_MANAGER_ERR("line %i : missing } in commandline reference '%s'",lineNr,value); }
              else *_tmp = 0;
              if (strlen(_dflt) < 0) { CONF_MANAGER_ERR("line %i : dflt value {} in commandline reference '%s' is too short!",lineNr,value); }
            }
            if (_descr!=nullptr) {
              *_descr = 0;
              _descr++;
              char *_tmp = strchr(_descr,']');
              if (_tmp == nullptr) { CONF_MANAGER_ERR("line %i : missing ] at end of commandline reference '%s'",lineNr,value); }
              else *_tmp = 0;
              if (strlen(_descr) < 0) { CONF_MANAGER_ERR("line %i : description :..] in commandline reference '%s' is too short!",lineNr,value); }
            }
            char *_tmp = strchr(_long,']');
            if (_tmp != nullptr) { *_tmp = 0; }
            char __s=0;
            if (_short!=nullptr) __s = *_short;
            char * trailing = endOfOption + 1;
            if (endOfOption == nullptr || *trailing == 0 || endOfOption > value + vl) {
              trailing = nullptr;
            }
            if ( (_dflt==nullptr)&&(_descr==nullptr) ) { // old option
              // do nothing...
            } else { // new option
              if (!cmdparser->optionExists(_long)) {
                if (ty==CFTP_NUM) {
                  char *ep=nullptr;
                  if (_dflt == nullptr) { CONF_MANAGER_ERR("line %i : to add a new commandline option you must specify a default value for it! To use an existing option, specify no default value AND no description text.",lineNr); }
                  else {
                    double _dfltD = strtod(_dflt,&ep);
                    if ((_dfltD==0.0)&&(ep==_dflt)) { CONF_MANAGER_ERR("line %i : invalid numerical default value for commandline reference '%s'",lineNr,value); }
                    cmdparser->addDouble( _long, __s, _descr, _dfltD );
                  }
                } else {
                  cmdparser->addStr( _long, __s, _descr, _dflt );
                }
                // Re-parsing must be enabled here, even if it's inefficient. Otherwise we are not able to read the newly created commandline options into the config.
                cmdparser->doParse(1);  // ignore duplicates...
              } else {
                SMILE_WRN(3, "configManager: commandline option %s already defined (not adding new), consider removing default value and description from config file.", _long);
              }
            }
            
            if (ty==CFTP_NUM) {
              if (trailing != nullptr) {
                tmpstr = myvprint("%f%s",cmdparser->getDouble(_long),trailing);
              } else {
                tmpstr = myvprint("%f",cmdparser->getDouble(_long));
              }
            } else {
              const char *_tmps = cmdparser->getStr(_long);
              if (_tmps == nullptr) {
                SMILE_ERR(1,"configManager: commandline option '%s' has nullptr as default value, please check the \\cm options in the config file (at least one for each option has to have a default value or description given!)",_long);
                if (trailing != nullptr) {
                  tmpstr = strdup(trailing);
                } else {
                  tmpstr = strdup("");
                }
              } else {
                if (trailing != nullptr) {
                  tmpstr = myvprint("%s%s", _tmps, trailing);
                } else {
                  tmpstr = strdup(_tmps);
                }
              }
            }
            value = tmpstr;
          } else {
            CONF_MANAGER_ERR("commandline reference specified in line %i, however no commandline parser is present!",lineNr);
          }
        }
      }
//TOOD: look for memory leaks here....:!?  value / tmpstr... 

    // parse value (remove pre and post whitespaces)
    l = (int)strlen(value);
    //remove spaces at end and beginning
    while (((value[l] == ' ')||(value[l] == '\t'))&&(l>0)) { value[l] = 0; l--; }
    while (((value[0] == ' ')||(value[0] == '\t'))&&(l>0)) { value[0] = 0; value++; l--; }
    if (l==0) CONF_PARSER_ERR("(line %i) cFileConfigReader::getInstance: empty value in line %i '%s' of instance '%s'",lineNr,i,line,inst[idx].name);
    SMILE_DBG(8,"       value=%s",value);



      int nFields = 1;
      char **valuefield = nullptr;
      if (arrT==10) {
        // count number of unquoted ';' separators
        int i;
        //while (value[0] == ';') { value++; l--; }
        //while (value[l] == ';') { value[l]=0; l--; }
        for (i=1; i<=l; i++) { if ((value[i] == ';')&&(value[i-1] != '\\')&&(i!=l)) nFields++; if (value[i] == 0) break; }

        // split into field array..
        valuefield = (char**)calloc(1,sizeof(char*)*nFields);
        i=0;
        do {
          if (i>=nFields) break;
          a = strchr(value,';');
          if (a!=nullptr) *a=0;
          valuefield[i]=value;
          int lll = (int)strlen(valuefield[i]);
          // remove spaces at beginning and end
          while ( (lll>=0)&& ((valuefield[i][0]==' ')||(valuefield[i][0]=='\t')) ) { valuefield[i]++; lll--; }
          while ( (lll>=0)&& ((valuefield[i][lll-1]==' ')||(valuefield[i][lll-1]=='\t')) ) { valuefield[i][lll-1]=0; lll--; }
          if (lll<=0) valuefield[i]=nullptr;
          if (a!=nullptr) {
            value=a+1;
          } 
          i++;
        } while (a!=nullptr);
      }

      int idx;
      for( idx=0; idx < nFields; idx++)  {

      if (arrT == 10) value = valuefield[idx] ; //...
      

      if (value == nullptr) continue;

//-----> was here.....

      int mtmp=0;

      vl = (int)strlen(value);
      for (mtmp=1; mtmp<vl; mtmp++) {
        if (value[mtmp-1]=='\\') {

               if ( (value[mtmp]==';')||(value[mtmp]=='\\') ) {

                  // remove value[a-1] and move all chars to left, vl--
                  int aa;
                  for (aa=mtmp-1; aa<vl; aa++) {
                    value[aa] = value[aa+1];
                  }
                  value[vl--] = 0;

               }

          }
      }
      // ---

      
      ConfigValue *cv=nullptr;
      double f;
      // convert value according to type...
      switch (ty) {
             //TODO: resolve variable references (i.e. $xxxx)
             // introduce ConfigValueRef type... which will be replaced once all instances have been read
        case CFTP_NUM:
          //sscanf(value,"%f",&f);
          char *eptr;
//        cv = new ConfigValueRef(REF_NUM,name)

          f = strtod(value,&eptr);
          if ((f==0.0)&&(eptr == value)) { CONF_PARSER_ERR("(line %i) cFileConfigReader::getInstance: error parsing numeric value '%s'",lineNr,value); }
          SMILE_DBG(8,"parsing numeric value: (field: '%s') val=%f (str: '%s')",field,f,value);
          cv = new ConfigValueNum(f);
          break;
        case CFTP_STR:
//        cv = new ConfigValueRef(REF_STR,name)
          cv = new ConfigValueStr(value);
          SMILE_DBG(8,"set string value : '%s'",cv->getStr());
          break;
        case CFTP_CHR:
//        cv = new ConfigValueRef(REF_CHR,name)
          if (!(strncmp(value,"<space>",7))) {
            cv = new ConfigValueChr(' ');
          } else if (!(strncmp(value,"<tab>",5))) {
            cv = new ConfigValueChr('\t');
          } else {
            if ((strlen(value) > 1)) SMILE_WRN(2,"(line %i) cFileConfigReader::getInstance: char field has length > 1 ('%s'='%s')",lineNr,field,value);
            cv = new ConfigValueChr(value[0]);
          }
          SMILE_DBG(8,"set char value : '%c'",cv->getChar());
          break;
        case CFTP_OBJ:
                 // TODO!!  resolve object references '\instancename'
        // also use ConfigValueRef type here...
//        cv = new ConfigValueRef(REF_INST,name)
                 // if (cman!=nullptr) in = cman->getInstance(iname);
//                 cv = new ConfigValueObj( in , 0 );
/*
          if (strlen(value) > 1) SMILE_WRN(2,"cFileConfigReader::getInstance: char field has length > 1 ('%s'='%s')",field,value);
          cv = new ConfigValueChr(value[0]);
          SMILE_DBG(5,"set char value : '%c'",cv->getChr());
          break;
*/
        default:
          CONF_PARSER_ERR("(line %i) cFileConfigReader::getInstance: unknown field type (name '%s') (in ConfigType '%s') ty=%i",lineNr,field,_type->getName(),ty);
          //CONF_PARSER_ERR("unknown type OR  cannot convert value");
      }

      //if (tmpstr!=nullptr) free(tmpstr); // <-----

      // TODO: variables, references, placeholders....
      
      // use setValue() to set the value
      if (arrT == 10) {
        char *c = myvprint("%s[%i]",field,idx);
//printf("setvalue10 %i '%s' = '%s'\n",idx,c,value); fflush(stdout);
        ret->setValue(cv, -1, c);
        free(c);
      } else {
//printf("setvalue %i %i '%s'\n",arrT,arrN,field);
        ret->setValue(cv, -1, field);
      }

      } // END for nFields

      if (tmpstr!=nullptr) free(tmpstr);
      // free valuefield:
      if (valuefield != nullptr) free(valuefield);
      
      //const ConfigValue *v = ret->getValue(-1,field);
      //SMILE_DBG(5,"str = '%s'",v->getStr());
    }
    free(origline);
  }
  return ret;
}



/******************************************************************************/



cConfigManager::cConfigManager(cCommandlineParser *parser) :
  nTypes(0), 
  nInst(0),
  nReaders(0),
  cmdparser(parser)
{
  defaults = (ConfigInstance **)calloc(1,sizeof(ConfigInstance *)*NEL_ALLOC_BLOCK);
  if (defaults != nullptr) nTypesAlloc = NEL_ALLOC_BLOCK;
  else nTypesAlloc = 0;

  inst = (ConfigInstance **)calloc(1,sizeof(ConfigInstance *)*NEL_ALLOC_BLOCK);
  if (inst != nullptr) nInstAlloc = NEL_ALLOC_BLOCK;
  else nInstAlloc = 0;

  reader = (cConfigReader **)calloc(1,sizeof(cConfigReader *)*NEL_ALLOC_BLOCK);
  //readerPriority = (int *)calloc(1,sizeof(int)*NEL_ALLOC_BLOCK);
//  if ((reader != nullptr)&&(readerPriority!=nullptr)) {
  if (reader != nullptr) {
    nReadersAlloc = NEL_ALLOC_BLOCK;
  } else {
    if (reader != nullptr) free(reader);
 //   if (readerPriority != nullptr) free(readerPriority);
    nReadersAlloc = 0;
  }
  externalObjectMap_ = new std::map <std::string, void *>();
}

/* order in which readers are added determines their priority,
   readers added last, will overwrite values from readers added prior to them */
int cConfigManager::addReader(cConfigReader *_reader)
{
  if (_reader == nullptr) return -1;
  if (nReaders >= nReadersAlloc) { // realloc if true
    cConfigReader **tmp = (cConfigReader **)realloc(reader, sizeof(cConfigReader*) * (nReadersAlloc+NEL_ALLOC_BLOCK));
    if (tmp != nullptr) {
      reader = tmp;
      nReadersAlloc += NEL_ALLOC_BLOCK;
    } else { OUT_OF_MEMORY; }
  }
  _reader->setCmdparser(cmdparser);
  reader[nReaders] = _reader;
  return nReaders++;
}

int cConfigManager::addInstance(ConfigInstance *_inst)
{
  if (_inst == nullptr) return -1;
  if (nInst >= nInstAlloc) { // realloc if true
    ConfigInstance **tmp = (ConfigInstance **)realloc(inst, sizeof(ConfigInstance*) * (nInstAlloc+NEL_ALLOC_BLOCK));
    if (tmp != nullptr) {
      inst = tmp;
      nInstAlloc += NEL_ALLOC_BLOCK;
    } else { OUT_OF_MEMORY; }
  }
  inst[nInst] = _inst;
  return nInst++;
}

int cConfigManager::deleteInstance(const char *_instname)   /* deletes instance "_instname" */
{
  int idx = findInstance(_instname);
  if (idx >= 0) {
	delete inst[idx];
	int i;
	for (i=idx; i<nInst-1; i++) {
      inst[i] = inst[i+1];
	}
	inst[i] = nullptr;
	nInst--;
	return 1;
  } else {
    SMILE_ERR(1,"cannot delete instance '%s' -> not found!",_instname);
  }
  return 0;
}

/* return value is index of instance or -1 if instance was not found */
int cConfigManager::findInstance(const char *_instname) const
{
  int i;
  if (_instname == nullptr) {
    SMILE_DBG(7,"findInstance called with _instname = nullptr!!");
    return -1;
  }
  for (i=0; i<nInst; i++) {
    SMILE_DBG(7,"findInstance: comparing instname ('%s') with '%s' for inst %i",_instname,inst[i]->getName(),i);
    if (!strcmp(inst[i]->getName(),_instname)) {
      SMILE_DBG(7,"findInstance: match (%i)!",i);
      return i;
    }
  }
  return -1;
}

/* return value is index of instance or -1 if instance was not found */
int cConfigManager::findType(const char *_typename) const
{
  int i;
  if (_typename == nullptr) return -1;
  for (i=0; i<nTypes; i++) {
    if (defaults[i]->getType() != nullptr) {
      if (!strcmp(defaults[i]->getTypeName(),_typename)) return i;
    }
  }
  return -1;
}

const ConfigType * cConfigManager::getTypeObj(int n) const
{
  if ((n>=0)&&(n<nTypes)) {
    if (defaults[n] != nullptr) return defaults[n]->getType();
  }
  return nullptr;
}

const ConfigType *cConfigManager::getTypeObj(const char *_typename) const // hierarchical type resolving...
{
  if (_typename != nullptr) {
    char *b=nullptr;
    const char *s=nullptr;
    const ConfigType *tp=nullptr;
    int h = instNameSplit(_typename, &b, &s);
    if (b!=nullptr) {
      tp = getTypeObj(findType(b));
      free(b); b=nullptr;
    }
    
    if (tp !=nullptr) {
      while (h==1) {
        h = instNameSplit(s, &b, &s);
        if (b!=nullptr) {
          int t = tp->findField(b);
          free(b); b=nullptr;
          tp = tp->getTypeObj(t);
          if (tp==nullptr) {
            SMILE_WRN(4,"getType: cannot find configType for '%s' (at 's=%s')",_typename,s);
            return nullptr;
          }
        } else {
          CONF_MANAGER_ERR("getType: empty base returned by instNameSplit! (for config type '%s')",_typename);
        }
      }
      return tp;
    } else {
      SMILE_WRN(4,"getType: cannot find configType base of '%s'",_typename);
      return nullptr;
    }
  }
  return nullptr;
}


int cConfigManager::updateInstance(ConfigInstance *_inst)
{
  if (_inst == nullptr) return -1;
  int update = 0;
  int idx = findInstance(_inst->getName());
  if (idx == -1) {
    // new instance, check for default instance in *defaults
    SMILE_DBG(6,"updateInstance: adding new instance '%s'",_inst->getName());
    const ConfigType *tmpType = _inst->getType();
    int tidx = findType(tmpType->getName());
    if (tidx == -1) {
      // error: unknown type! (can not be...)
      CONF_MANAGER_ERR("updateInstance: trying to add instance (%s) of unknown type (%s) to configManager",_inst->getName(),tmpType->getName());
    } else {
      // copy and "update" default:
      _inst->missingFrom(defaults[tidx]);
      addInstance(_inst);
    }
  } else {
    //update only
    inst[idx]->updateWith(_inst);
    update = 1;
  }
  return update;
}

/* register an instance specifying the type and the default values */
/* the instance MUST contain a type, the instance must be created with freeType=0
   the instance will be freed by the configManager */
int cConfigManager::registerType(ConfigInstance *_type)
{
  if (_type == nullptr) return -1;
  // check for unique name... TODO:
  int existing = findType(_type->getName());
  if (existing >= 0) {
    SMILE_WRN(3,"ConfigType '%s' is already registered. Exiting cConfigManager::registerType",_type->getName());
    delete _type;
    return existing;
  }
  if (nTypes >= nTypesAlloc) { // realloc if true
    ConfigInstance **tmp = (ConfigInstance **)realloc(defaults, sizeof(ConfigInstance*) * (nTypesAlloc+NEL_ALLOC_BLOCK));
    if (tmp != nullptr) {
      defaults = tmp;
      nTypesAlloc += NEL_ALLOC_BLOCK;
    } else { OUT_OF_MEMORY; }
  }
  defaults[nTypes] = _type;
  return nTypes++;
}

void cConfigManager::readConfig()
{                     /* read the config, after readers and types have been registered */
  // find instance names for all types in all readers
  int i,r,t;
  for (t=0; t<nTypes; t++) {  // foreach config type
    const ConfigType *typ = defaults[t]->getType();
    for (r=0; r<nReaders; r++) { // foreach reader, in priority order
      int _n;
      char ** inames = reader[r]->findInstances(typ, &_n); // find instances
      SMILE_DBG(7,"found %i instance(s) for type %s",_n,typ->getName());
      if (inames != nullptr) {
        for (i=0; i<_n; i++) { // foreach instance of the current reader
          SMILE_DBG(7,"reading configInstance %i ('%s') from reader %i",i,inames[i],r);
          ConfigInstance *rd = reader[r]->getInstance(inames[i], typ);

/*      #ifdef DEBUG
      const ConfigValue *v=rd->getValue(-1,"age");
      if (v!=nullptr) {
        SMILE_DBG(4,"readConfig: inst->getValue('age') = %i",v->getInt());
      }
      #endif*/
              // TODO: if getInstance returned nullptr , check for instance link... remember link


          // now add the instance to the Manager
          if (updateInstance(rd))
            { delete rd; }
          free(inames[i]);
        }
        free(inames);
      } else {
        SMILE_DBG(6,"findInstances returned nullptr pointer to instnames array!");
      }

    }


  }
  // TODO: now, for each instance: resolve wildcards, %cmdline_xx%, etc. (except for %instname% )

    // TODO: scan for instance links
    // ... and for variable links (... difficult...)

}

/* no hierarchical names are supported... */
ConfigInstance * cConfigManager::getInstance(const char *_instname)
{
  const char * _subname;
  char * _instbasename=nullptr;
  int h = instNameSplit(_instname, &_instbasename, &_subname);
  int idx = findInstance(_instbasename);
  if (_instbasename != nullptr) free(_instbasename);
  if (idx >= 0) {
    if (h) { // search through the hierarchy
      CONF_MANAGER_ERR("cConfigManager::getInstance: cannot get sub-instance, use getValue instead!");
    } else {
      return inst[idx];
    }
  }
  return nullptr;
}

const ConfigValue * cConfigManager::getValue(const char *_name) const
{
  const char * _subname=nullptr;
  char * _instbasename=nullptr;

  SMILE_DBG(7,"cConfigManager::getValue: _name = '%s'",_name);

  int h = instNameSplit(_name, &_instbasename, &_subname);
  SMILE_DBG(7,"cConfigManager::getValue: instNameSplit returned %i",h);
  int idx = findInstance(_instbasename);
  if (_instbasename != nullptr) free(_instbasename);
  if (idx >= 0) {
    if (h) { // search through the hierarchy
      return inst[idx]->getValue(-1,_subname);
    } else {
      CONF_MANAGER_ERR("field name not given in name '%s'",_name);
    }
  } else CONF_MANAGER_ERR("base instance of field '%s' not found in configManager!",_name);
  return nullptr;
}

int cConfigManager::getArraySize(const char *_name) const
{
  const ConfigValue *v = getValue(_name);
  if (v!=nullptr) {
    if (v->getType() >= CFTP_ARR) {
      return v->getSize();
    } else {
      CONF_MANAGER_ERR("cannot get size of array field '%s', this field is not an array (type=%i)",_name,v->getType());
    }
  }
  return -1;
}

char ** cConfigManager::getArrayKeys(const char *_name, int *N) const
{
  const ConfigValue *v = getValue(_name);
  if (v!=nullptr) {
    if (v->getType() >= CFTP_ARR) {
      if (N!=nullptr) *N = v->getSize();
      return v->getAAkeys();
    } else {
      CONF_MANAGER_ERR("cannot get names (and size) of array field '%s', this field is not an array (type=%i)",_name,v->getType());
    }
  }
  if (N!=nullptr) *N=0;
  return nullptr;
}

void cConfigManager::printTypeHelp(int _subtype, const char *selection, int dfltConf)
{
  int i;
  const ConfigType * tp;

  if (defaults == nullptr) return;
  for (i=0; i<nTypes; i++) {
    tp = defaults[i]->getType();
    if (tp != nullptr) {
      if (selection != nullptr) {
        //printf("N: %s Sel: %s Strl(sel): %i\n",tp->getName(), selection, strlen(selection));
        if (!strncasecmp(tp->getName(), selection, strlen(selection)) ) {
          tp->printTypeHelp(nullptr,_subtype);
          if (dfltConf) {
//            SMILE_PRINT(" // default (template) configuration file section for '%s' >>",tp->getName());
            tp->printTypeDfltConfig(nullptr,_subtype);
            //SMILE_PRINT("");
          }
        }
      } else {
        tp->printTypeHelp(nullptr,_subtype);
        if (dfltConf) {
          //SMILE_PRINT(" // default (template) configuration file section for '%s' >>",tp->getName());
          tp->printTypeDfltConfig(nullptr,_subtype);
          //SMILE_PRINT("");
        }
      }
    }
  }
}

/*
fullMode == 1:   selection contains a list of multiple components,
                 create a full config file with component list,
                 though leaving reader/writer levels blank!
*/
void cConfigManager::printTypeDfltConfig(const char *selection, int _subtype, int fullMode, int withDescription)
{
  int i;
  const ConfigType * tp;

  if (defaults == nullptr) return;
  if (fullMode) {
    if (selection == nullptr) {
      SMILE_ERR(1,"cannot print a template config file (fullMode==1), when no comma separated list of components is given!");
      return;
    }
    // parse comma separated list of component types (names can appear twice..)
    // and remove all spaces adjacent to a comma
    int nSel = 1;
    long l = (long)strlen(selection);
    // TODO: check for empty string, or just consisting of spaces/tabs ..
    for (i=0; i<l; i++) {
      if (selection[i] == ',') nSel++;
      if (selection[i] == 0) break;
    }
    
    char * mySel = strdup(selection);
    const char ** sels = (const char**) calloc(1,sizeof(const char*) * nSel);

    int s = 0; i=0;
    sels[s] = mySel; while (sels[s][0] == ' ') { sels[s]++; i++; }
    for (; i<l; i++) {
      if (selection[i] == ',') {
        s++; mySel[i++] = 0;
        if (s>=nSel) break; // this break should never be reached, just for double safety...
        sels[s] = mySel+i; while (sels[s][0] == ' ') { sels[s]++; i++; }
      }
    }

    // print the header and a few comments
    SMILE_PRINT("\n ///////////////////////////////////////////////////////////////////////////");
    SMILE_PRINT(" // openSMILE configuration template file generated by SMILExtract binary //");
    SMILE_PRINT(" // you must manually adapt this file and change at least the             //");
    SMILE_PRINT(" // 'reader/writer.dmLevel =' lines.                                      //");
    SMILE_PRINT(" ///////////////////////////////////////////////////////////////////////////\n");

    SMILE_PRINT(" ;===== component manager configuration (= list of enabled components!) =====\n");

    SMILE_PRINT("[componentInstances:cComponentManager]");
    SMILE_PRINT(" // this line configures the default data memory:");
    SMILE_PRINT("instance[dataMemory].type = cDataMemory");
    for (i=0; i<nSel; i++) {
      tp = getTypeObj(findType(sels[i]));
      if (tp == nullptr) {
        SMILE_ERR(1,"Type '%s' not found! It seems, that this component does not exist!",sels[i]);
      } else {
        char * modStr = strdup(sels[i]+1);
        if (modStr != nullptr) {
          modStr[0] = tolower(modStr[0]);
          SMILE_PRINT("instance[%s].type = %s",modStr,sels[i]);
          free(modStr);
        }
      }
    }
    SMILE_PRINT(" // Here you can control the amount of detail displayed for the data memory\n  // level configuration. 0 is no information at all, 5 is maximum detail.");
    SMILE_PRINT("printLevelStats = 1");
    SMILE_PRINT(" // You can set the number of parallel threads (experimental):");
    SMILE_PRINT("nThreads = 1");

    SMILE_PRINT("\n// ============= component configuration sections begin here ==============\n");

    // for each element in the list print the config section
    for (i=0; i<nSel; i++) {
      tp = getTypeObj(findType(sels[i]));
      if (tp != nullptr) tp->printTypeDfltConfig(nullptr,_subtype,withDescription);
      else {
        SMILE_ERR(1,"Type '%s' not found! It seems, that this component does not exist!",sels[i]);
      }
    }

    free(sels);
    free(mySel);

    SMILE_PRINT("\n// ################### END OF openSMILE CONFIG FILE ######################\n");
    
  } else {

    for (i=0; i<nTypes; i++) {
      tp = defaults[i]->getType();
      if (tp != nullptr) {
          if ((selection != nullptr)&&(!fullMode)) {
            //printf("N: %s Sel: %s Strl(sel): %i\n",tp->getName(), selection, strlen(selection));
            if (!strncasecmp(tp->getName(), selection, strlen(selection)) ) {
              tp->printTypeDfltConfig(nullptr,_subtype,withDescription);
            }
          } else {
            tp->printTypeDfltConfig(nullptr,_subtype,withDescription);
          }
      }
    }

  }
}

cConfigManager::~cConfigManager()
{
  int i;
  //free instances
  if (inst != nullptr) {
    for (i=0; i<nInst; i++) {
      if (inst[i] != nullptr) { delete inst[i]; }
    }
    free(inst);
  }
  nInstAlloc=0; nInst=0;

  //free types
  if (defaults != nullptr) {
    for (i=0; i<nTypes; i++) {
      if (defaults[i] != nullptr) { delete defaults[i]; }
    }
    free(defaults);
  }
  nTypesAlloc=0; nTypes=0;

  //free readers
  if (reader != nullptr) {
    for (i=0; i<nReaders; i++) {
      if (reader[i] != nullptr) { delete reader[i]; }
    }
    free(reader);
  }
  nReadersAlloc=0; nReaders=0;
  if (externalObjectMap_ != nullptr)
    delete externalObjectMap_;
}

