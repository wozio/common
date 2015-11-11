#include "logger.h"
#include "discovery.h"
#include "yamicontainer.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <string>
#include <iostream>
#include <list>
#include <fstream>

using namespace boost::posix_time;

namespace home_system
{
namespace logger
{

std::string _log_file_path("logger.log");
std::stringstream _log_stream;
std::mutex _mutex;

struct log_entry
{
  level level_;
  std::string msg_;
};

std::list<log_entry> _log;


// TODO: when more stuff to be done on constructor use this struct instead of
// bare ofstream
struct log_file
{
  std::ofstream f_;
  log_file()
  {
    f_.open(_log_file_path, std::ios_base::out | std::ios_base::app);
  }
};

void log(level l, std::stringstream& stream)
{
  static std::ofstream f(_log_file_path, std::ios_base::out | std::ios_base::app);
  _log.push_back({l, stream.str() });
  switch (l)
  {
    case level::debug:
      f << "[DEBUG] ";
      break;
    case level::info:
      f << "[INFO]  ";
      break;
    case level::warning:
      f << "[WARN]  ";
      break;
    case level::error:
      f << "[ERROR] ";
      break;
  }
  
  std::string t = to_iso_extended_string(microsec_clock::local_time());
  
  f << t << ": " << stream.str() << std::endl;
  std::cout << t << ": " << stream.str() << std::endl;
  stream.str("");
  f.flush();
}

/*void configure(const char* file_name, const std::string& log_level, bool console_log)
{
  Poco::Logger &l = Poco::Logger::root();

  Poco::AutoPtr<Poco::FileChannel> sfc(new Poco::FileChannel(file_name));
  sfc->setProperty("rotation","1 M");
  sfc->setProperty("purgeCount","10");

  Poco::AutoPtr<Poco::PatternFormatter> pf(new Poco::PatternFormatter);
  pf->setProperty("pattern", "[%q] %Y-%m-%d %H:%M:%S:%i (%I)%s: %t");

  Poco::AutoPtr<Poco::FormattingChannel> fc(new Poco::FormattingChannel(pf));

  if (console_log)
  {
    Poco::AutoPtr<Poco::SplitterChannel> sc(new Poco::SplitterChannel);
    Poco::AutoPtr<Poco::ConsoleChannel> cc(new Poco::ConsoleChannel);

    sc->addChannel(cc);
    sc->addChannel(sfc);

    fc->setChannel(sc);
  }
  else
  {
    fc->setChannel(sfc);
  }

  l.setChannel(fc);
  l.setLevel(log_level);
}*/

}  
}
