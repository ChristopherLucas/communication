#include "crcppdatabase.h"

#include <Rcpp.h>
using namespace Rcpp;
#include <core/smileCommon.hpp>

#include <core/configManager.hpp>
#include <core/commandlineParser.hpp>
#include <core/componentManager.hpp>

#define MODULE "CrcppDataBase"


CRcppDataBase::CRcppDataBase():
  modeWork {cComponentManager::NoRccp}
{ 
}

void CRcppDataBase::getData1file()
{
  
}

int CRcppDataBase::work1file(std::vector<std::string> arguments)
{
  int argc = arguments.size() + 1;
  char ** argv = new char*[argc];
  const char * c_argv_0 = "NoExe";
  argv[0] = const_cast<char *>(c_argv_0);
  for(int i = 0; i<arguments.size(); i++)
  {
    argv[i+1] = new char[arguments[i].length() + 1];
    strcpy(argv[i+1], arguments[i].c_str());    
  }
  try {
    
    smileCommon_fixLocaleEnUs();
    
    // set up the smile logger
    LOGGER.setLogLevel(1);
    LOGGER.enableConsoleOutput();
    
    
    // commandline parser:
    cCommandlineParser cmdline(argc,const_cast<const char **>(argv));
    cmdline.addStr( "configfile", 'C', "Path to openSMILE config file", "smile.conf" );
    cmdline.addInt( "loglevel", 'l', "Verbosity level (0-9)", 2 );
#ifdef DEBUG
    cmdline.addBoolean( "debug", 'd', "Show debug messages (on/off)", 0 );
#endif
    cmdline.addInt( "nticks", 't', "Number of ticks to process (-1 = infinite) (only works for single thread processing, i.e. nThreads=1)", -1 );
    //cmdline.addBoolean( "configHelp", 'H', "Show documentation of registered config types (on/off)", 0 );
    cmdline.addBoolean( "components", 'L', "Show component list", 0 );
    cmdline.addStr( "configHelp", 'H', "Show documentation of registered config types (on/off/argument) (if an argument is given, show only documentation for config types beginning with the name given in the argument)", nullptr, 0 );
    cmdline.addStr( "configDflt", 0, "Show default config section templates for each config type (on/off/argument) (if an argument is given, show only documentation for config types beginning with the name given in the argument, OR for a list of components in conjunctions with the 'cfgFileTemplate' option enabled)", nullptr, 0 );
    cmdline.addBoolean( "cfgFileTemplate", 0, "Print a complete template config file for a configuration containing the components specified in a comma separated string as argument to the 'configDflt' option", 0 );
    cmdline.addBoolean( "cfgFileDescriptions", 0, "Include description in config file templates.", 0 );
    cmdline.addBoolean( "ccmdHelp", 'c', "Show custom commandline option help (those specified in config file)", 0 );
    cmdline.addStr( "logfile", 0, "set log file", "smile.log" );
    cmdline.addBoolean( "nologfile", 0, "don't write to a log file (e.g. on a read-only filesystem)", 0 );
    cmdline.addBoolean( "noconsoleoutput", 0, "don't output any messages to the console (log file is not affected by this option)", 0 );
    cmdline.addBoolean( "appendLogfile", 0, "append log messages to an existing logfile instead of overwriting the logfile at every start", 0 );
    
    int help = 0;
    if (cmdline.doParse() == -1) {
      LOGGER.setLogLevel(0);
      help = 1;
    }
    if (argc <= 1) {
      Rprintf("\nNo commandline options were given.\n Please run ' SMILExtract -h ' to see some usage information!\n\n");
      return EXIT_ERROR;
    }
    
    if (help==1) { return EXIT_ERROR; }
    
    if (cmdline.getBoolean("nologfile")) {
      LOGGER.setLogFile((const char *)nullptr,0,!(cmdline.getBoolean("noconsoleoutput")));
    } else {
      LOGGER.setLogFile(cmdline.getStr("logfile"),cmdline.getBoolean("appendLogfile"),!(cmdline.getBoolean("noconsoleoutput")));
    }
    LOGGER.setLogLevel(cmdline.getInt("loglevel"));
    SMILE_MSG(2,"openSMILE starting!");
    
#ifdef DEBUG  // ??
    if (!cmdline.getBoolean("debug"))
      LOGGER.setLogLevel(LOG_DEBUG, 0);
#endif
    
    SMILE_MSG(2,"config file is: %s",cmdline.getStr("configfile"));
    
    
    // create configManager:
    cConfigManager *configManager = new cConfigManager(&cmdline);
    
    cComponentManager *cMan = new cComponentManager(configManager, modeWork, componentlist);
    
    
    const char *selStr=nullptr;
    if (cmdline.isSet("configHelp")) {
#ifndef EXTERNAL_BUILD
      selStr = cmdline.getStr("configHelp");
      configManager->printTypeHelp(1/*!!! -> 1*/,selStr,0);
#endif
      help = 1;
    }
    if (cmdline.isSet("configDflt")) {
#ifndef EXTERNAL_BUILD
      int fullMode=0; 
      int wDescr = 0;
      if (cmdline.getBoolean("cfgFileTemplate")) fullMode=1;
      if (cmdline.getBoolean("cfgFileDescriptions")) wDescr=1;
      selStr = cmdline.getStr("configDflt");
      configManager->printTypeDfltConfig(selStr,1,fullMode,wDescr);
#endif
      help = 1;
    }
    if (cmdline.getBoolean("components")) {
#ifndef EXTERNAL_BUILD
      cMan->printComponentList();
#endif  // EXTERNAL_BUILD
      help = 1;
    }
    
    if (help==1) {
      delete configManager;
      delete cMan;
      return EXIT_ERROR; 
    }
    
    
    // TODO: read config here and print ccmdHelp...
    // add the file config reader:
    try{ 
      configManager->addReader( new cFileConfigReader( cmdline.getStr("configfile"), -1, &cmdline) );
      configManager->readConfig();
    } catch (cConfigException *cc) {
      return EXIT_ERROR;
    }
    
    /* re-parse the command-line to include options created in the config file */
    cmdline.doParse(1,0); // warn if unknown options are detected on the commandline
    if (cmdline.getBoolean("ccmdHelp")) {
      cmdline.showUsage();
      delete configManager;
      delete cMan;
      return EXIT_ERROR;
    }
    
    /* create all instances specified in the config file */
    cMan->createInstances(0); // 0 = do not read config (we already did that above..)
    
    /*
     MAIN TICK LOOP :
     */
    cmanGlob = cMan;
    
    /* run single or mutli-threaded, depending on componentManager config in config file */
    long long nTicks = cMan->runMultiThreaded(cmdline.getInt("nticks"));
    getData1file();
    /* it is important that configManager is deleted BEFORE componentManger! 
     (since component Manger unregisters plugin Dlls, which might have allocated configTypes, etc.) */
    delete configManager;
    delete cMan;
    
  } catch(cSMILException *c) { 
    // free exception ?? 
    return EXIT_ERROR; 
  } 
  
  return EXIT_SUCCESS;  
}
