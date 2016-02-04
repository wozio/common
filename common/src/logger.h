#ifndef LOGGER_H
#define	LOGGER_H

#include <boost/log/trivial.hpp>

namespace home_system
{

void init_log(const char* file, bool console_log);

}

#define LOGDEBUG(msg) BOOST_LOG_TRIVIAL(debug) << msg
#define LOGINFO(msg)  BOOST_LOG_TRIVIAL(info) << msg
#define LOGWARN(msg)  BOOST_LOG_TRIVIAL(warning) << msg
#define LOGERROR(msg) BOOST_LOG_TRIVIAL(error) << msg
#define LOG(msg) LOGDEBUG(msg)

#endif	/* LOGGER_H */

