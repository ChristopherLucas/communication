#ifndef CRCPPDATABASE_H
#define CRCPPDATABASE_H

#include <core/componentManager.hpp>

class CRcppDataBase
{ 
public:
  CRcppDataBase();
  cComponentManager::RcppModeWork modeWork;
  int work1file(std::vector<std::string> arguments);
protected:
  virtual void getData1file();
  cComponentManager *cmanGlob {nullptr};
};
  
#endif // CRCPPDATABASE_H
