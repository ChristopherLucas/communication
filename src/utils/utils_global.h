#ifndef UtilsGlobal_H
#define UtilsGlobal_H

#include <stdio.h>
#include <string>

//only for windows
#include "io_win32.h"

#if ( defined(__MINGW32__) )
#define fopen_speech(file, mode) io::win32::fopen( (file), (mode))
#else    
#define fopen_speech(file, mode) fopen( (file), (mode))
#endif

std::string tildaString(const std::string & normalString);

#endif // UtilsGlobal_H