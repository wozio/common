#ifndef LOGGER_H
#define	LOGGER_H

#define ELPP_THREAD_SAFE
#ifdef __GNUC__
#define ELPP_STACKTRACE_ON_CRASH
#endif
#define ELPP_NO_DEFAULT_LOG_FILE
#define ELPP_WINSOCK2

#include "easylogging++.h"

#endif	/* LOGGER_H */

