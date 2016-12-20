// Copyright Maciej Sobczak 2008-2015.
// This file is part of YAMI4.
//
// YAMI4 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// YAMI4 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

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
