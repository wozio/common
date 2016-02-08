#include "logger_init.h"

#include <boost/filesystem.hpp>
#include <string>

namespace home_system
{

void init_log(const char* file, bool console_log)
{
  std::string path("/var/log/home-system/");
  boost::filesystem::create_directories(path);
  
  el::Configurations defaultConf;
  defaultConf.setToDefault();
  defaultConf.setGlobally(el::ConfigurationType::Filename, path + file);
  defaultConf.setGlobally(el::ConfigurationType::MaxLogFileSize, std::to_string(1 * 1024 * 1024));
  defaultConf.setGlobally(el::ConfigurationType::LogFlushThreshold, "10");
  defaultConf.setGlobally(el::ConfigurationType::ToFile, "true");
  defaultConf.setGlobally(el::ConfigurationType::ToStandardOutput, console_log ? "true" : "false");
  defaultConf.setGlobally(el::ConfigurationType::Format, "[%datetime] [%levshort] [%thread] [%loc] %msg");
  el::Loggers::reconfigureLogger("default", defaultConf);
  el::Loggers::addFlag(el::LoggingFlag::StrictLogFileSizeCheck);
}

}