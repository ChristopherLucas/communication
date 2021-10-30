#include "utils_global.h"
#include <cctype>
#include <algorithm>
#include <iostream>
#include <string>
#include <sys/stat.h> // stat
#include <errno.h>    // errno, ENOENT, EEXIST
#if defined(_WIN32)
#include <direct.h>   // _mkdir
#endif

namespace speech
{
  FILE* fopen(const char* path, const char* mode)
  {
  #if ( defined(__MINGW32__) )
    setlocale(LC_ALL, " ");
    return io::win32::fopen(path, mode);
  #else    
    return std::fopen(path, mode);
  #endif
  }

  std::string str_tolower(const std::string & s_) {
    std::string s=s_;
    std::transform(s.begin(), s.end(), s.begin(), 
                   [](unsigned char c){ return std::tolower(c); }
    );
    return s;
  }
  
  bool str_ends_with(std::string const & value, std::string const & ending)
  {
    if (ending.size() > value.size()) return false;
    return std::equal(ending.rbegin(), ending.rend(), value.rbegin());
  }

  bool isDirExist(const std::string& path)
  {
  #if defined(_WIN32)
    struct _stat info;
    if (_stat(path.c_str(), &info) != 0)
    {
      return false;
    }
    return (info.st_mode & _S_IFDIR) != 0;
  #else 
    struct stat info;
    if (stat(path.c_str(), &info) != 0)
    {
      return false;
    }
    return (info.st_mode & S_IFDIR) != 0;
  #endif
  }

  bool makePath(const std::string& path)
  {
  #if defined(_WIN32)
    int ret = _mkdir(path.c_str());
  #else
    mode_t mode = 0755;
    int ret = mkdir(path.c_str(), mode);
  #endif
    if (ret == 0)
      return true;
    
    switch (errno)
    {
    case ENOENT:
      // parent didn't exist, try to create it
    {
      int pos = path.find_last_of('/');
      if (pos == std::string::npos)
  #if defined(_WIN32)
        pos = path.find_last_of('\\');
      if (pos == std::string::npos)
  #endif
        return false;
      if (!makePath( path.substr(0, pos) ))
        return false;
    }
      // now, try to create again
  #if defined(_WIN32)
      return 0 == _mkdir(path.c_str());
  #else 
      return 0 == mkdir(path.c_str(), mode);
  #endif
      
    case EEXIST:
      // done!
      return isDirExist(path);
      
    default:
      return false;
    }
  }


  filepath::filepath (const std::string & str): std::string(tildaString(str))
  {
    
  }

  std::string filepath::string()
  {
    return *this;
  }
  
  std::string filepath::tildaString(const std::string & normalString)
  {
    std::string resString = normalString;
    if(!normalString.empty())
    {
      if('~' == normalString.at(0))
      {
        //std::size_t pos = normalString.find("/");
        //if(std::string::npos == pos)
        //  pos = normalString.find("\\");  
        //std::string tilda_prefix = normalString.substr(pos);
        resString.replace(0,1, std::getenv("HOME"));
      }
    }
    return resString;
  }

  filepath_vector::filepath_vector (const std::vector<std::string> & vector): std::vector<std::string>(tildaString(vector))
  {
    
  }

  std::vector<std::string> filepath_vector::vector_string()
  {
    return *this;  
  }

  std::vector<std::string> filepath_vector::tildaString(const std::vector<std::string> & normalVector)
  {
    std::vector<std::string> res;
    for(int i=0; i<normalVector.size(); i++)
    {
      filepath fp(normalVector[i]);
      res.push_back(fp.string());
    }
    return res;
  }

};
