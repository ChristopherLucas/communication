#ifndef ID3_DECODER_H
#define ID3_DECODER_H

#include <sys/stat.h> // For stat | TODO: Replace with c++ equivalent
#include <string>
#include <fstream>

struct ID3_Header {
  char ident[3];
  int8_t major_version;
  int8_t revision;
  char flags;
  char size[4];
};

class ID3 {
public:
  ID3(std::string filename);
  ~ID3();
  bool hasTag();
  int32_t size();
private:
  ID3_Header getHeader();
  int32_t syncIntConv(int32_t num);
  //std::ifstream file;
  FILE * file {nullptr};
  long int flen;
};


#endif /* ID3_DECODER_H */