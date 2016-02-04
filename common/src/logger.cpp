#include "logger.h"

#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/expressions.hpp>
#include <boost/log/sources/severity_logger.hpp>
#include <boost/log/sources/record_ostream.hpp>
#include <boost/log/utility/setup/file.hpp>
#include <boost/log/utility/setup/common_attributes.hpp>
#include <boost/log/support/date_time.hpp>
#include <boost/filesystem.hpp>

namespace logging = boost::log;
namespace src = boost::log::sources;
namespace expr = boost::log::expressions;
namespace keywords = boost::log::keywords;
namespace attrs = boost::log::attributes;

namespace home_system
{

void init_log(const char* file, bool console_log)
{
  if (!console_log)
  {
    boost::log::add_common_attributes();

    std::string path("/var/log/home-system/");
    boost::filesystem::create_directories(path);
    path += file;
    path += "_%N.log";
    boost::log::add_file_log(
        keywords::file_name = path,
        keywords::rotation_size = 2 * 1024 * 1024,
        keywords::auto_flush = true,
        keywords::format =
        (
          expr::stream
            << expr::format_date_time< boost::posix_time::ptime >("TimeStamp", "%Y-%m-%d %H:%M:%S.%f") << " "
            << expr::attr<attrs::current_thread_id::value_type>("ThreadID") << " "
            << logging::trivial::severity << ": "
            << expr::smessage
        )
    );
  }
}

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
