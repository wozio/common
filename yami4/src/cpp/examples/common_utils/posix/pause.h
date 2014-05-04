#ifndef YAMICPP_PAUSE_H_INCLUDED
#define YAMICPP_PAUSE_H_INCLUDED

#include <unistd.h>

namespace examples
{

// suspend for one second
void pause()
{
    sleep(1);
}

} // namespace examples

#endif // YAMICPP_PAUSE_H_INCLUDED
