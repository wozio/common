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

#include "selector.h"
#include "../allocator.h"
#include "../channel.h"
#include "../io_error_handler.h"
#include "../listener.h"

#include <cstring>
#include <errno.h>

using namespace yami;
using namespace details;

core::result selector::init(allocator & alloc)
{
    io_error_callback_ = NULL;

    alloc_ = &alloc;
    polls_ = NULL;

    core::result res;

    polls_size_ = 10; // initial size of poll array
    polls_ = static_cast<pollfd *>(
        alloc_->allocate(sizeof(struct pollfd) * polls_size_));
    if (polls_ != NULL)
    {
        polls_used_ = 0;

        io_descriptor_type pipe_descriptors[2];

        int cc = ::pipe(pipe_descriptors);
        if (cc == 0)
        {
            interrupt_pipe_write_ = pipe_descriptors[1];
            interrupt_pipe_read_ = pipe_descriptors[0];

            res = core::ok;
        }
        else
        {
            alloc_->deallocate(polls_);
            polls_ = NULL;

            res = core::io_error;
        }
    }
    else
    {
        res = core::no_memory;
    }

    return res;
}

void selector::install_io_error_logger(
    core::io_error_function io_error_callback,
    void * io_error_callback_hint)
{
    io_error_callback_ = io_error_callback;
    io_error_callback_hint_ = io_error_callback_hint;
}

void selector::clean()
{
    ::close(interrupt_pipe_write_);
    ::close(interrupt_pipe_read_);

    if (polls_ != NULL)
    {
        alloc_->deallocate(polls_);
        polls_ = NULL;
    }
}

void selector::reset()
{
    // always add the internal interrupt pipe

    polls_[0].fd = interrupt_pipe_read_;
    polls_[0].events = POLLIN;

    polls_used_ = 1;

#ifdef YAMI4_WITH_OPEN_SSL
    ssl_pending_ = false;
#endif // YAMI4_WITH_OPEN_SSL
}

core::result selector::grow_polls()
{
    core::result res;

    std::size_t new_size = 2 * polls_size_;

    pollfd * new_array = static_cast<pollfd *>(
        alloc_->allocate(sizeof(struct pollfd) * new_size));
    if (new_array != NULL)
    {
        std::memcpy(new_array, polls_, sizeof(struct pollfd) * polls_size_);

        alloc_->deallocate(polls_);

        polls_ = new_array;
        polls_size_ = new_size;

        res = core::ok;
    }
    else
    {
        res = core::no_memory;
    }

    return res;
}

core::result selector::add_channel(channel & ch,
    bool allow_outgoing_traffic, bool allow_incoming_traffic)
{
    core::result res = core::ok;

    if (polls_used_ == polls_size_)
    {
        res = grow_polls();
    }

    if (res == core::ok)
    {
        io_descriptor_type fd;
        io_direction direction;
        
        ch.get_io_descriptor(fd, direction);
        
        pollfd & pf = polls_[polls_used_];
        pf.events = 0;

#ifdef YAMI4_WITH_OPEN_SSL
        ch.set_pending_read(false);
#endif // YAMI4_WITH_OPEN_SSL

        if ((direction == input || direction == inout) && allow_incoming_traffic)
        {
            pf.fd = fd;
            pf.events |= POLLIN;

#ifdef YAMI4_WITH_OPEN_SSL
            SSL * ssl = ch.get_ssl();
            if (ssl != NULL)
            {
                int pending = SSL_pending(ssl);
                if (pending != 0)
                {
                    ssl_pending_ = true;

                    ch.set_pending_read(true);
                }
            }
#endif // YAMI4_WITH_OPEN_SSL
        }

        if ((direction == output || direction == inout) && allow_outgoing_traffic)
        {
            pf.fd = fd;
            pf.events |= POLLOUT;
        }

        ch.set_selector_index(polls_used_);
        
        ++polls_used_;
    }

    return res;
}

core::result selector::add_listener(listener & lst)
{
    core::result res = core::ok;

    if (polls_used_ == polls_size_)
    {
        res = grow_polls();
    }

    if (res == core::ok)
    {
        io_descriptor_type fd = lst.get_io_descriptor();

        pollfd & pf = polls_[polls_used_];
        pf.fd = fd;
        pf.events = POLLIN;

        lst.set_selector_index(polls_used_);
    
        ++polls_used_;
    }

    return res;
}

void selector::get_channel_usage(int & max_allowed, int & used)
{
    max_allowed = 0;
    used = polls_used_;
}

core::result selector::wait(std::size_t timeout)
{
    core::result res;

#ifdef YAMI4_WITH_OPEN_SSL
    if (ssl_pending_)
    {
        // if there are any bytes pending in SSL buffers,
        // we do not want to wait on select, but still want
        // to poll the state of all file descriptors

        timeout = 0;
    }
#endif // YAMI4_WITH_OPEN_SSL

    const int cc = ::poll(polls_, polls_used_, static_cast<int>(timeout));

    if (cc > 0)
    {
        // the wait might have been interrupted with the internal pipe
        // if that is the case, consume the dummy info

        // the internal pipe always has the first slot in the polls_ array

        if ((polls_[0].revents & POLLIN) != 0)
        {
            bool keep_trying = true;
            while (keep_trying)
            {
                char dummy;
                ssize_t rn =
                    ::read(interrupt_pipe_read_, &dummy, sizeof(dummy));

                if (rn == -1)
                {
                    // if the attempt to read is abandoned due to signal,
                    // it should be repeated
                    // otherwise it was an error

                    if (errno != EINTR)
                    {
                        handle_io_error("selector pipe read",
                            io_error_callback_, io_error_callback_hint_);

                        res = core::io_error;
                        keep_trying = false;
                    }
                }
                else
                {
                    res = core::ok;
                    keep_trying = false;
                }
            }
        }
        else
        {
            // the wait was interrupted because there is
            // a regular event to process

            res = core::ok;
        }
    }
    else if (cc == 0)
    {
#ifdef YAMI4_WITH_OPEN_SSL
        if (ssl_pending_)
        {
            // the timeout == 0 expired, but since there are some
            // pending bytes in SSL buffers, we consider it as
            // a valid outcome of the selection process

            res = core::ok;
        }
        else
        {
            res = core::timed_out;
        }
#else // YAMI4_WITH_OPEN_SSL

        res = core::timed_out;

#endif // YAMI4_WITH_OPEN_SSL
    }
    else
    {
        if (cc == EINTR)
        {
            // the wait operation will be repeated in the outer loop
            
            res = core::ok;
        }
        else
        {
            handle_io_error("poll wait",
                io_error_callback_, io_error_callback_hint_);

            res = core::io_error;
        }
    }

    return res;
}

bool selector::is_channel_ready(
    const channel & ch, io_direction & direction) const
{
    io_descriptor_type fd;
    io_direction dir;

    ch.get_io_descriptor(fd, dir);

    bool ready_for_reading = false;
    bool ready_for_writing = false;

    int index = ch.get_selector_index();

    if (index >= 0)
    {
        pollfd & pf = polls_[index];
        
        if (dir == input || dir == inout)
        {
            if ((pf.revents & POLLIN) != 0)
            {
                ready_for_reading = true;
            }
            
#ifdef YAMI4_WITH_OPEN_SSL
            if (ch.get_pending_read())
            {
                ready_for_reading = true;
            }
#endif // YAMI4_WITH_OPEN_SSL
        }
        
        if (dir == output || dir == inout)
        {
            if ((pf.revents & POLLOUT) != 0)
            {
                ready_for_writing = true;
            }
        }
        
        if (ready_for_reading && ready_for_writing)
        {
            direction = inout;
        }
        else if (ready_for_reading)
        {
            direction = input;
        }
        else if (ready_for_writing)
        {
            direction = output;
        }
    }

    return ready_for_reading || ready_for_writing;
}

bool selector::is_listener_ready(const listener & lst) const
{
    bool ready = false;
    
    int index = lst.get_selector_index();
    
    if (index >= 0)
    {
        pollfd & pf = polls_[index];
        
        ready = (pf.revents & POLLIN) != 0;
    }
    
    return ready;
}

core::result selector::interrupt()
{
    // wake the poll up from its current wait state

    core::result res = core::ok;

    bool keep_trying = true;
    while (keep_trying)
    {
        char dummy = '\0';
        ssize_t written =
            ::write(interrupt_pipe_write_, &dummy, sizeof(dummy));

        if (written == -1)
        {
            // if the attempt to write is abandoned due to signal,
            // it should be repeated
            // otherwise it was an error

            if (errno != EINTR)
            {
                handle_io_error("poll pipe write",
                    io_error_callback_, io_error_callback_hint_);

                res = core::io_error;
                keep_trying = false;
            }
        }
        else
        {
            res = core::ok;
            keep_trying = false;
        }
    }

    return res;
}
