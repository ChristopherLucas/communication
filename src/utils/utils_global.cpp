#include "utils_global.h"


std::string tildaString(const std::string & normalString)
{
  std::string resString = normalString;
  if('~' == normalString.at(0))
  {
    //std::size_t pos = normalString.find("/");
    //if(std::string::npos == pos)
    //  pos = normalString.find("\\");  
    //std::string tilda_prefix = normalString.substr(pos);
    resString.replace(0,1, std::getenv("HOME"));
  }
  return resString;
}