#ifndef YAMICORE_FLAG_H_INCLUDED
#define YAMICORE_FLAG_H_INCLUDED

#include <Windows.h>
#include <cassert>

namespace examples
{

class flag
{
public:
    flag()
    {
        event_ = CreateEvent(NULL, TRUE, FALSE, NULL);
        assert(event_ != NULL);
    }

    ~flag()
    {
        CloseHandle(event_);
    }

    void notify()
    {
        BOOL cc = SetEvent(event_);
        assert(cc);
    }

    void wait()
    {
        DWORD cc = WaitForSingleObject(event_, INFINITE);
        assert(cc == WAIT_OBJECT_0);
    }

    bool get_value()
    {
        DWORD cc = WaitForSingleObject(event_, 0);
        assert(cc == WAIT_OBJECT_0 || cc == WAIT_TIMEOUT);
        return cc == WAIT_OBJECT_0;
    }

private:
    HANDLE event_;
};

} // namespace examples

#endif // YAMICORE_FLAG_H_INCLUDED
