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

#include <cassert>
#include <pthread.h>

namespace examples
{

extern "C" typedef void (*thread_function)(void * arg);

struct starter_arguments
{
    thread_function tf;
    void * tf_arg;
};

// POSIX thread function used as a starter for the actual function
extern "C" void * thread_starter_function(void * arg)
{
    starter_arguments * sa = static_cast<starter_arguments *>(arg);

    thread_function tf = sa->tf;
    void * tf_arg = sa->tf_arg;

    delete sa;

    tf(tf_arg);

    return NULL;
}

void start_thread(thread_function tf, void * arg)
{
    starter_arguments * sa = new starter_arguments;
    sa->tf = tf;
    sa->tf_arg = arg;

    pthread_t th;
    int cc = pthread_create(&th, NULL, &thread_starter_function, sa);
    assert(cc == 0);

    cc = pthread_detach(th);
    assert(cc == 0);
}

} // namespace examples
