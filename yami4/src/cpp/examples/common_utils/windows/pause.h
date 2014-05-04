#ifndef YAMICPP_PAUSE_H_INCLUDED
#define YAMICPP_PAUSE_H_INCLUDED

#include <Windows.h>

namespace examples
{

// suspend for one second
void pause()
{
    Sleep(1000);
}

} // namespace examples

#endif // YAMICPP_PAUSE_H_INCLUDED
