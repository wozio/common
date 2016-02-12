#include "logger_init.h"

#include <boost/filesystem.hpp>
#include <string>

namespace home_system
{

void init_log(const char* file, bool console_log)
{
#ifdef __linux__
  std::string path("/var/log/home-system/");
  std::string to_file("true");
  try
  {
    boost::filesystem::create_directories(path);
  }
  catch (...)
  {
    to_file = "false";
  }
#else
  to_file = "false";
#endif
  
  el::Configurations defaultConf;
  defaultConf.setToDefault();
  defaultConf.setGlobally(el::ConfigurationType::Filename, path + file);
  defaultConf.setGlobally(el::ConfigurationType::MaxLogFileSize, std::to_string(1 * 1024 * 1024));
  defaultConf.setGlobally(el::ConfigurationType::LogFlushThreshold, "10");
  defaultConf.setGlobally(el::ConfigurationType::ToFile, to_file);
  defaultConf.setGlobally(el::ConfigurationType::ToStandardOutput, console_log ? "true" : "false");
  defaultConf.setGlobally(el::ConfigurationType::Format, "[%datetime] [%levshort] [%thread] [%fbase:%line] %msg");
  el::Loggers::reconfigureLogger("default", defaultConf);
  el::Loggers::addFlag(el::LoggingFlag::StrictLogFileSizeCheck);
}

}