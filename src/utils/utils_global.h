#ifndef UtilsGlobal_H
#define UtilsGlobal_H

#include <stdio.h>
#include <string>
#include <vector>

//only for windows
#include "io_win32.h"
  
namespace speech
{
  FILE* fopen(const char* path, const char* mode);

  std::string str_tolower(const std::string & s);
  bool str_ends_with(std::string const & value, std::string const & ending);

  class filepath : public std::string
  {
  public:
    filepath (const std::string & str);
    std::string string();
  private:
    std::string tildaString(const std::string & normalString);
  };

  class filepath_vector : public std::vector <std::string>
  {
  public:
    filepath_vector (const std::vector<std::string> & vector);
    filepath_vector() = default;
    filepath_vector(const filepath_vector & vector) = default;
    filepath_vector & operator= (const filepath_vector & vector) = default;
    std::vector<std::string> vector_string();
  private:
    std::vector<std::string> tildaString(const std::vector<std::string> & normalVector);   
  };
};

#endif // UtilsGlobal_H