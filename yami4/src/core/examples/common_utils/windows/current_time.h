#ifndef YAMICORE_CURRENT_TIME_H_INCLUDED
#define YAMICORE_CURRENT_TIME_H_INCLUDED

#include <sys/timeb.h>

namespace examples
{

// returns current time in milliseconds
long long clock()
{
    _timeb now;

    _ftime(&now);

    return static_cast<long long>(now.time) * 1000 + now.millitm / 1000;
}

} // namespace examples

#endif // YAMICORE_CURRENT_TIME_H_INCLUDED
