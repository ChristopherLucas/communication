#ifndef __SMILE_UTIL_CPP_H
#define __SMILE_UTIL_CPP_H

#ifndef __SMILE_COMMON_H
#define __SMILE_COMMON_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>

#endif  // __SMILE_COMMON_H

#include "core/smileTypes.h"
#include "smileutil/smileUtil.h"

/* 
 read the wave file header from fileHandle, store parameters in struct pointed to by pcmParam 
 the file must be opened via fopen()
 */
int smilePcm_readWaveHeader(FILE *filehandle, sWaveParameters *pcmParam, const char *filename);




#endif  // __SMILE_UTIL_CPP_H