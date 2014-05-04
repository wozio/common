#ifndef YAMICORE_CURRENT_TIME_H_INCLUDED
#define YAMICORE_CURRENT_TIME_H_INCLUDED

#include <sys/time.h>

namespace examples
{

// returns current time in milliseconds
long long clock()
{
    timeval tv;

    gettimeofday(&tv, NULL);

    return static_cast<long long>(tv.tv_sec) * 1000 + tv.tv_usec / 1000;
}

} // namespace examples

#endif // YAMICORE_CURRENT_TIME_H_INCLUDED
