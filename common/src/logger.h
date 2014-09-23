#ifndef LOGGER_H
#define	LOGGER_H

#include <sstream>
#include <mutex>

namespace home_system
{
namespace logger
{

enum class level {
  debug,
  info,
  warning,
  error
};

extern std::stringstream _log_stream;
extern std::string _log_file_path;

void log(level l, std::stringstream& stream);

extern std::mutex _mutex;

}
}

#define LOGIMPL(l, msg) {\
  std::lock_guard<std::mutex> lock(home_system::logger::_mutex);\
  home_system::logger::_log_stream << __FILE__ << ": " << __LINE__ << ": " << msg;\
  log(l, home_system::logger::_log_stream);\
}

#define LOGDEBUG(msg) LOGIMPL(home_system::logger::level::debug,   msg)
#define LOGINFO(msg)  LOGIMPL(home_system::logger::level::info,    msg)
#define LOGWARN(msg)  LOGIMPL(home_system::logger::level::warning, msg)
#define LOGERROR(msg) LOGIMPL(home_system::logger::level::error,   msg)
#define LOG(msg) LOGDEBUG(msg)

#endif	/* LOGGER_H */

