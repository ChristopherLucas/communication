#include <sys/stat.h> // For stat | TODO: Replace with c++ equivalent
#include <string>
#include <cstring>
#include <fstream>
#include <stdlib.h>
#include <bitset>
#include <vector>

#include "id3.h"
#include "utils_global.h"

//only for windows
#include "io_win32.h"

#include <Rcpp.h>
using namespace Rcpp;


ID3::ID3(std::string filename)
{
  file = speech::fopen(filename.c_str(), "rb");
  
  if( nullptr == file )
    throw std::string("ID3: can not open input wave file");  

  struct stat fstat;
  if (stat(filename.c_str(), &fstat) != 0)
    throw std::string("ID3: Error opening file! (stat)");
  
  flen = fstat.st_size;
}

bool ID3::hasTag()
{
  char magic[3];
  memset(magic, 0, 3);
  fseek(file, 0, SEEK_SET);
  int read =0;
  read = fread(magic, sizeof(char), 3, file);
  return strncmp( magic, "ID3", 3 ) == 0;
}

ID3_Header ID3::getHeader()
{
  ID3_Header header;
  if(!hasTag()) // Maybe throw exception
    return header;
  fseek(file, 0, SEEK_SET);
  int read =0;
  read = fread((char*)&header, sizeof(char), 10, file);  
  return header;
}

int32_t ID3::syncIntConv(int32_t num)
{
  // Reverse Byte Order
  unsigned char bytes[4];
  bytes[0] = (num >> 24) & 0xFF;
  bytes[1] = (num >> 16) & 0xFF;
  bytes[2] = (num >>  8) & 0xFF;
  bytes[3] =  num        & 0xFF;
  num = *((int32_t*)bytes);
  
  std::bitset<sizeof(int32_t)*8> source(num);
  std::bitset<sizeof(int32_t)*8> target;
  
  unsigned int targetCounter = 0;
  for(unsigned int i = 0; i < sizeof(int32_t)*8; ++i)
  {
    if((i+1) % 8 != 0)
    {
      target.set(targetCounter, source[i]);
      targetCounter++; // Set bits from back to beginning to convert to msb order
    }
  }
  
  return target.to_ulong();
}

int32_t ID3::size()
{
  ID3_Header header;
  if(hasTag())
  {
    header = getHeader();
    return syncIntConv(*((int32_t*)header.size)) + sizeof(ID3_Header);
  }
  else
    return 0;
  
}

ID3::~ID3()
{
  fclose(file);
}