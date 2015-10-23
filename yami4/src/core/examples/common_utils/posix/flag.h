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

#include <pthread.h>
#include <cassert>

namespace examples
{

class flag
{
public:
    flag()
    {
        value_ = false;

        int cc = pthread_mutex_init(&mtx_, NULL);
        assert(cc == 0);

        cc = pthread_cond_init(&cond_, NULL);
        assert(cc == 0);
    }

    ~flag()
    {
        int cc = pthread_mutex_destroy(&mtx_);
        assert(cc == 0);

        cc = pthread_cond_destroy(&cond_);
        assert(cc == 0);
    }

    void notify()
    {
        int cc = pthread_mutex_lock(&mtx_);
        assert(cc == 0);

        value_ = true;

        cc = pthread_mutex_unlock(&mtx_);
        assert(cc == 0);

        cc = pthread_cond_signal(&cond_);
        assert(cc == 0);
    }

    void wait()
    {
        int cc = pthread_mutex_lock(&mtx_);
        assert(cc == 0);

        while (value_ == false)
        {
            cc = pthread_cond_wait(&cond_, &mtx_);
            assert(cc == 0);
        }

        cc = pthread_mutex_unlock(&mtx_);
        assert(cc == 0);
    }

    bool get_value()
    {
        int cc = pthread_mutex_lock(&mtx_);
        assert(cc == 0);

        bool result = value_;

        cc = pthread_mutex_unlock(&mtx_);
        assert(cc == 0);

        return result;
    }

private:
    bool value_;
    pthread_mutex_t mtx_;
    pthread_cond_t cond_;
};

} // namespace examples

#endif // YAMICORE_FLAG_H_INCLUDED
