#include "logger_init.h"

#include <boost/filesystem.hpp>
#include <string>

namespace home_system
{

void init_log(const char* file, bool console_log)
{
#ifndef DISABLE_LOGS
  std::string to_file("false");
  std::string path("/var/log/home-system/");
#ifdef __linux__
  try
  {
    boost::filesystem::create_directories(path);
    to_file = "true";
  }
  catch (...)
  {
    to_file = "false";
  }
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
#endif
}

}