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

#include <Python.h>

#include <yami4-cpp/yami.h>

#include <deque>
#include <string>
#include <exception>

#ifdef WIN32
#include <Windows.h>
#else
#include <pthread.h>
#endif // WIN32

namespace // unnamed
{

struct result
{
    result(void * p) : success_(true), p_(p) {}
    result(int i) : success_(true), i_(i) {}
    result(void * p, int i) : success_(true), p_(p), i_(i) {}
    result(int i, int j, int k) : success_(true), i_(i), j_(j), k_(k) {}
    result(const std::string & s) : success_(true), s_(s) {}
    result(const std::exception &e) : success_(false), error_(e.what()) {}

    bool success_;
    void * p_;
    int i_;
    int j_;
    int k_;
    std::string s_;
    std::string error_;
};

template <typename T>
void * make_result(const T & t)
{
    try
    {
        result * res = new result(t);
        return res;
    }
    catch (...)
    {
        return NULL;
    }
}

void * make_result(int i, int j, int k)
{
    try
    {
        result * res = new result(i, j, k);
        return res;
    }
    catch (...)
    {
        return NULL;
    }
}

void * make_result(void * p, int i)
{
    try
    {
        result * res = new result(p, i);
        return res;
    }
    catch (...)
    {
        return NULL;
    }
}

// helper wrapper that bundles together the agent and message dispatch queue
#ifdef WIN32

// implementation for Windows
struct agent_wrapper
{
    agent_wrapper(yami::agent * a, std::size_t max_queue_length)
        : a_(a), max_queue_length_(max_queue_length), terminated_(false)
    {
        InitializeCriticalSection(&mutex_);
        nonempty_semaphore_ = CreateSemaphore(NULL, 0, INT_MAX, NULL);
        nonfull_semaphore_ = CreateSemaphore(NULL,
            static_cast<LONG>(max_queue_length), INT_MAX, NULL);
    }

    ~agent_wrapper()
    {
        DeleteCriticalSection(&mutex_);
        CloseHandle(nonempty_semaphore_);
        CloseHandle(nonfull_semaphore_);
        delete a_;
    }

    // incoming message handler
    void operator()(yami::incoming_message & im)
    {
        yami::incoming_message * stored_msg = new yami::incoming_message(im);

        {
            if (terminated_ == false)
            {
                WaitForSingleObject(nonfull_semaphore_, INFINITE);

                EnterCriticalSection(&mutex_);

                messages_.push_back(stored_msg);

                LeaveCriticalSection(&mutex_);

                ReleaseSemaphore(nonempty_semaphore_, 1, NULL);
            }
        }
    }

    // connection event handler
    void operator()(const std::string & name, yami::connection_event event)
    {
        if (terminated_ == false)
        {
            std::string event_description;
            switch (event)
            {
            case yami::new_incoming_connection:
                event_description = "i ";
                break;
            case yami::new_outgoing_connection:
                event_description = "o ";
                break;
            case yami::connection_closed:
                event_description = "c ";
                break;
            }

            event_description += name;

            WaitForSingleObject(nonfull_semaphore_, INFINITE);

            EnterCriticalSection(&mutex_);

            connection_events_.push_back(event_description);

            LeaveCriticalSection(&mutex_);

            ReleaseSemaphore(nonempty_semaphore_, 1, NULL);
        }
    }

    void terminate_queue()
    {
        EnterCriticalSection(&mutex_);

        terminated_ = true;

        LeaveCriticalSection(&mutex_);
        ReleaseSemaphore(nonempty_semaphore_, 1, NULL);
    }

    yami::incoming_message * get_next_incoming_message()
    {
        yami::incoming_message * stored_msg;

        {
            WaitForSingleObject(nonempty_semaphore_, INFINITE);

            EnterCriticalSection(&mutex_);

            if (messages_.empty() == false)
            {
                stored_msg = messages_.front();
                messages_.pop_front();
            }
            else
            {
                // the item of interest is in the connection event queue
                // or this is the termination condition
                stored_msg = NULL;
            }

            LeaveCriticalSection(&mutex_);

            ReleaseSemaphore(nonfull_semaphore_, 1, NULL);
        }

        return stored_msg;
    }

    // returns empty string if no event happened
    std::string get_next_connection_event()
    {
        std::string result;
        {
            EnterCriticalSection(&mutex_);

            if (connection_events_.empty() == false)
            {
                result = connection_events_.front();
                connection_events_.pop_front();
            }

            LeaveCriticalSection(&mutex_);
        }

        return result;
    }

    yami::agent * a_;
    std::deque<yami::incoming_message *> messages_;
    std::deque<std::string> connection_events_;
    const std::size_t max_queue_length_;
    bool terminated_;

    CRITICAL_SECTION mutex_;
    HANDLE nonempty_semaphore_;
    HANDLE nonfull_semaphore_;
};

#else

// implementation for POSIX systems
struct agent_wrapper
{
    agent_wrapper(yami::agent * a, std::size_t max_queue_length)
        : a_(a), max_queue_length_(max_queue_length), terminated_(false)
    {
        pthread_mutex_init(&mutex_, NULL);
        pthread_cond_init(&condvar_, NULL);
    }

    ~agent_wrapper()
    {
        pthread_mutex_destroy(&mutex_);
        pthread_cond_destroy(&condvar_);
        delete a_;
    }

    void operator()(yami::incoming_message & im)
    {
        yami::incoming_message * stored_msg = new yami::incoming_message(im);

        {
            pthread_mutex_lock(&mutex_);

            if (terminated_ == false)
            {
                while (messages_.size() >= max_queue_length_)
                {
                    pthread_cond_wait(&condvar_, &mutex_);
                }

                messages_.push_back(stored_msg);
            }

            pthread_mutex_unlock(&mutex_);
            pthread_cond_signal(&condvar_);
        }
    }

    // connection event handler
    void operator()(const std::string & name, yami::connection_event event)
    {
        if (terminated_ == false)
        {
            std::string event_description;
            switch (event)
            {
            case yami::new_incoming_connection:
                event_description = "i ";
                break;
            case yami::new_outgoing_connection:
                event_description = "o ";
                break;
            case yami::connection_closed:
                event_description = "c ";
                break;
            }

            event_description += name;

            pthread_mutex_lock(&mutex_);

            while (messages_.size() >= max_queue_length_)
            {
                pthread_cond_wait(&condvar_, &mutex_);
            }

            connection_events_.push_back(event_description);

            pthread_mutex_unlock(&mutex_);
            pthread_cond_signal(&condvar_);
        }
    }

    void terminate_queue()
    {
        pthread_mutex_lock(&mutex_);

        terminated_ = true;

        pthread_mutex_unlock(&mutex_);
        pthread_cond_signal(&condvar_);
    }

    yami::incoming_message * get_next_incoming_message()
    {
        yami::incoming_message * stored_msg;

        {
            pthread_mutex_lock(&mutex_);

            while (terminated_ == false &&
                messages_.empty() && connection_events_.empty())
            {
                pthread_cond_wait(&condvar_, &mutex_);
            }

            if (messages_.empty() == false)
            {
                stored_msg = messages_.front();
                messages_.pop_front();
            }
            else
            {
                // the item of interest is in the connection event queue
                // or this is the termination condition
                stored_msg = NULL;
            }

            pthread_mutex_unlock(&mutex_);
            pthread_cond_signal(&condvar_);
        }

        return stored_msg;
    }

    // returns empty string if no event happened
    std::string get_next_connection_event()
    {
        std::string result;
        {
            pthread_mutex_lock(&mutex_);

            if (connection_events_.empty() == false)
            {
                result = connection_events_.front();
                connection_events_.pop_front();
            }

            pthread_mutex_unlock(&mutex_);
        }

        return result;
    }

    yami::agent * a_;
    std::deque<yami::incoming_message *> messages_;
    const std::size_t max_queue_length_;
    std::deque<std::string> connection_events_;
    bool terminated_;

    pthread_mutex_t mutex_;
    pthread_cond_t condvar_;
};
#endif // WIN32

} // unnamed namespace

extern "C" int yami4_is_success(void * pr)
{
    result * res = static_cast<result *>(pr);
    return res->success_ ? 1 : 0;
}

extern "C" void * yami4_get_pointer(void * pr)
{
    result * res = static_cast<result *>(pr);
    return res->p_;
}

extern "C" int yami4_get_int_i(void * pr)
{
    result * res = static_cast<result *>(pr);
    return res->i_;
}

extern "C" int yami4_get_int_j(void * pr)
{
    result * res = static_cast<result *>(pr);
    return res->j_;
}

extern "C" int yami4_get_int_k(void * pr)
{
    result * res = static_cast<result *>(pr);
    return res->k_;
}

extern "C" const char * yami4_get_string(void * pr)
{
    result * res = static_cast<result *>(pr);
    return res->s_.c_str();
}

extern "C" const char * yami4_get_error(void * pr)
{
    result * res = static_cast<result *>(pr);
    return res->error_.c_str();
}

extern "C" void yami4_destroy_result(void * pr)
{
    result * res = static_cast<result *>(pr);
    delete res;
}

extern "C" void * yami4_create_agent(
    const char * serialized_options, int buffer_len)
{
    try
    {
        yami::parameters options;
        if (serialized_options != NULL)
        {
            const char * buffers[1] = { serialized_options };
            std::size_t buffer_sizes[1] =
                { static_cast<std::size_t>(buffer_len) };
            options.deserialize(buffers, buffer_sizes, 1);
        }

        options.set_boolean(yami::option_names::deliver_as_raw_binary, true);

        // the incoming message queue has arbitrary default
        std::size_t max_queue_length = 100;
        yami::parameter_entry e;
        bool max_length_found = options.find("incoming_queue_max_length", e);
        if (max_length_found && e.type() == yami::integer)
        {
            max_queue_length = static_cast<std::size_t>(e.get_integer());
        }

        yami::agent * a = new yami::agent(options);

        agent_wrapper * wrapper = new agent_wrapper(a, max_queue_length);

        a->register_object("*", *wrapper);
        a->register_connection_event_monitor(*wrapper);

        return make_result(wrapper);
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void yami4_agent_terminate_incoming_queue(void * pa)
{
    agent_wrapper * wrapper = static_cast<agent_wrapper *>(pa);
    wrapper->terminate_queue();
}

extern "C" void yami4_destroy_agent(void * pa)
{
    agent_wrapper * wrapper = static_cast<agent_wrapper *>(pa);
    delete wrapper;
}

extern "C" void * yami4_agent_add_listener(void * pa,
    const char * listener)
{
    agent_wrapper * wrapper = static_cast<agent_wrapper *>(pa);
    yami::agent * a = wrapper->a_;
    try
    {
        std::string resolved_address = a->add_listener(listener);

        return make_result(resolved_address);
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void yami4_agent_remove_listener(void * pa,
    const char * listener)
{
    agent_wrapper * wrapper = static_cast<agent_wrapper *>(pa);
    yami::agent * a = wrapper->a_;
    try
    {
        a->remove_listener(listener);
    }
    catch (const std::exception & e)
    {
        // ignore
    }
}

extern "C" void * yami4_agent_open_connection(void * pa,
    const char * target)
{
    agent_wrapper * wrapper = static_cast<agent_wrapper *>(pa);
    yami::agent * a = wrapper->a_;
    try
    {
        a->open_connection(target);

        return make_result(1);
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void * yami4_agent_send(void * pa,
    const char * target, const char * object_name, const char * message_name,
    const char * serialized_content, int buffer_len,
    int priority, int auto_connect)
{
    agent_wrapper * wrapper = static_cast<agent_wrapper *>(pa);
    yami::agent * a = wrapper->a_;
    try
    {
        yami::raw_buffer_data_source raw_content(
            serialized_content, static_cast<std::size_t>(buffer_len));

        std::auto_ptr<yami::outgoing_message> msg(
            a->send(target, object_name, message_name,
                raw_content, priority, auto_connect));

        void * res = make_result(msg.get());
        msg.release();
        return res;
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void * yami4_agent_send_one_way(void * pa,
    const char * target, const char * object_name, const char * message_name,
    const char * serialized_content, int buffer_len,
    int priority, int auto_connect)
{
    agent_wrapper * wrapper = static_cast<agent_wrapper *>(pa);
    yami::agent * a = wrapper->a_;
    try
    {
        yami::raw_buffer_data_source raw_content(
            serialized_content, static_cast<std::size_t>(buffer_len));

        a->send_one_way(target, object_name, message_name,
            raw_content, priority, auto_connect);

        return make_result(1);
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void * yami4_agent_get_next_incoming_message(void * pa)
{
    agent_wrapper * wrapper = static_cast<agent_wrapper *>(pa);
    yami::incoming_message * msg = wrapper->get_next_incoming_message();
    if (msg != NULL)
    {
        return make_result(msg);
    }
    else
    {
        // special case, to be recognized by caller as end of queue
        return NULL;
    }
}

extern "C" void * yami4_agent_get_next_connection_event(void * pa)
{
    agent_wrapper * wrapper = static_cast<agent_wrapper *>(pa);

    const std::string event_description =
        wrapper->get_next_connection_event();

    return make_result(event_description);
}

extern "C" void yami4_agent_close_connection(void * pa,
    const char * target, int priority)
{
    agent_wrapper * wrapper = static_cast<agent_wrapper *>(pa);
    yami::agent * a = wrapper->a_;
    try
    {
        a->close_connection(target, priority);
    }
    catch (const std::exception & e)
    {
        // ignore
    }
}

extern "C" void yami4_agent_hard_close_connection(void * pa,
    const char * target)
{
    agent_wrapper * wrapper = static_cast<agent_wrapper *>(pa);
    yami::agent * a = wrapper->a_;
    try
    {
        a->hard_close_connection(target);
    }
    catch (const std::exception & e)
    {
        // ignore
    }
}

extern "C" void yami4_destroy_outgoing_message(void * pmsg)
{
    yami::outgoing_message * msg =
        static_cast<yami::outgoing_message *>(pmsg);
    delete msg;
}

extern "C" void * yami4_outgoing_message_get_state(void * pmsg)
{
    yami::outgoing_message * msg =
        static_cast<yami::outgoing_message *>(pmsg);
    try
    {
        std::size_t sent_bytes;
        std::size_t total_byte_count;
        yami::message_state state =
            msg->get_state(sent_bytes, total_byte_count);

        int res;
        switch (state)
        {
        case yami::posted:
            res = 1;
            break;
        case yami::transmitted:
            res = 2;
            break;
        case yami::abandoned:
            res = 3;
            break;
        case yami::replied:
            res = 4;
            break;
        case yami::rejected:
            res = 5;
            break;
        }

        return make_result(res,
            static_cast<int>(sent_bytes), static_cast<int>(total_byte_count));
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void * yami4_outgoing_message_wait_for_transmission(
    void * pmsg, int timeout)
{
    yami::outgoing_message * msg =
        static_cast<yami::outgoing_message *>(pmsg);
    try
    {
        bool result;
        if (timeout > 0)
        {
            result = msg->wait_for_transmission(
                static_cast<std::size_t>(timeout));
        }
        else
        {
            msg->wait_for_transmission();
            result = true;
        }

        return make_result(result ? 1 : 0);
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void * yami4_outgoing_message_wait_for_completion(
    void * pmsg, int timeout)
{
    yami::outgoing_message * msg =
        static_cast<yami::outgoing_message *>(pmsg);
    try
    {
        bool result;
        if (timeout > 0)
        {
            result = msg->wait_for_completion(
                static_cast<std::size_t>(timeout));
        }
        else
        {
            msg->wait_for_completion();
            result = true;
        }

        return make_result(result ? 1 : 0);
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void * yami4_outgoing_message_get_raw_reply(void * pmsg)
{
    yami::outgoing_message * msg =
        static_cast<yami::outgoing_message *>(pmsg);
    try
    {
        const std::vector<char> & reply = msg->get_raw_reply();

        return make_result(
            const_cast<char *>(&reply[0]), static_cast<int>(reply.size()));
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void * yami4_outgoing_message_get_exception_msg(void * pmsg)
{
    yami::outgoing_message * msg =
        static_cast<yami::outgoing_message *>(pmsg);
    try
    {
        const std::string & exc_msg = msg->get_exception_msg();

        return make_result(exc_msg);
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void * yami4_incoming_message_get_source(void * pmsg)
{
    yami::incoming_message * msg =
        static_cast<yami::incoming_message *>(pmsg);
    try
    {
        const std::string & source = msg->get_source();

        return make_result(source);
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void * yami4_incoming_message_get_object_name(void * pmsg)
{
    yami::incoming_message * msg =
        static_cast<yami::incoming_message *>(pmsg);
    try
    {
        const std::string & obj_name = msg->get_object_name();

        return make_result(obj_name);
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void * yami4_incoming_message_get_message_name(void * pmsg)
{
    yami::incoming_message * msg =
        static_cast<yami::incoming_message *>(pmsg);
    try
    {
        const std::string & msg_name = msg->get_message_name();

        return make_result(msg_name);
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void * yami4_incoming_message_get_raw_content(void * pmsg)
{
    yami::incoming_message * msg =
        static_cast<yami::incoming_message *>(pmsg);
    try
    {
        const std::vector<char> & content = msg->get_raw_content();

        return make_result(
            const_cast<char *>(&content[0]),
            static_cast<int>(content.size()));
    }
    catch (const std::exception & e)
    {
        return make_result(e);
    }
}

extern "C" void yami4_incoming_message_reply(void * pmsg,
    const char * serialized_content, int buffer_len,
    int priority)
{
    yami::incoming_message * msg =
        static_cast<yami::incoming_message *>(pmsg);
    try
    {
        yami::raw_buffer_data_source raw_content(
            serialized_content, static_cast<std::size_t>(buffer_len));

        msg->reply(raw_content, priority);
    }
    catch (const std::exception & e)
    {
        // ignore
    }
}

extern "C" void yami4_incoming_message_reject(void * pmsg,
    const char * reason, int priority)
{
    yami::incoming_message * msg =
        static_cast<yami::incoming_message *>(pmsg);
    try
    {
        msg->reject(reason, priority);
    }
    catch (const std::exception & e)
    {
        // ignore
    }
}

extern "C" void yami4_destroy_incoming_message(void * pmsg)
{
    yami::incoming_message * msg =
        static_cast<yami::incoming_message *>(pmsg);
    delete msg;
}

// thin wrappers for standard extension API

namespace // unnamed
{

unsigned long long ptr2ull(void * p)
{
    return *reinterpret_cast<unsigned long long *>(&p);
}

void * ull2ptr(unsigned long long v)
{
    return *reinterpret_cast<void * *>(&v);
}

} // unnamed namespace

static PyObject * yami4py_is_success(PyObject *self, PyObject *args)
{
    unsigned long long pr;
    if (!PyArg_ParseTuple(args, "K", &pr))
    {
        return NULL;
    }

    int res = yami4_is_success(ull2ptr(pr));

    return Py_BuildValue("i", res);
}

static PyObject * yami4py_get_pointer(PyObject *self, PyObject *args)
{
    unsigned long long pr;
    if (!PyArg_ParseTuple(args, "K", &pr))
    {
        return NULL;
    }

    void * res = yami4_get_pointer(ull2ptr(pr));

    if (res != NULL)
    {
        return Py_BuildValue("K", ptr2ull(res));
    }
    else
    {
        Py_RETURN_NONE;
    }
}

static PyObject * yami4py_get_int_i(PyObject *self, PyObject *args)
{
    unsigned long long pr;
    if (!PyArg_ParseTuple(args, "K", &pr))
    {
        return NULL;
    }

    int res = yami4_get_int_i(ull2ptr(pr));

    return Py_BuildValue("i", res);
}

static PyObject * yami4py_get_int_j(PyObject *self, PyObject *args)
{
    unsigned long long pr;
    if (!PyArg_ParseTuple(args, "K", &pr))
    {
        return NULL;
    }

    int res = yami4_get_int_j(ull2ptr(pr));

    return Py_BuildValue("i", res);
}

static PyObject * yami4py_get_int_k(PyObject *self, PyObject *args)
{
    unsigned long long pr;
    if (!PyArg_ParseTuple(args, "K", &pr))
    {
        return NULL;
    }

    int res = yami4_get_int_k(ull2ptr(pr));

    return Py_BuildValue("i", res);
}

static PyObject * yami4py_get_string(PyObject *self, PyObject *args)
{
    unsigned long long pr;
    if (!PyArg_ParseTuple(args, "K", &pr))
    {
        return NULL;
    }

    const char * res = yami4_get_string(ull2ptr(pr));

    return Py_BuildValue("y", res);
}

static PyObject * yami4py_get_error(PyObject *self, PyObject *args)
{
    unsigned long long pr;
    if (!PyArg_ParseTuple(args, "K", &pr))
    {
        return NULL;
    }

    const char * res = yami4_get_error(ull2ptr(pr));

    return Py_BuildValue("y", res);
}

static PyObject * yami4py_destroy_result(PyObject *self, PyObject *args)
{
    unsigned long long pr;
    if (!PyArg_ParseTuple(args, "K", &pr))
    {
        return NULL;
    }

    yami4_destroy_result(ull2ptr(pr));

    Py_RETURN_NONE;
}

static PyObject * yami4py_create_agent(PyObject *self, PyObject *args)
{
    const char * serialized_options;
    int dummy;
    int buffer_len;
    if (!PyArg_ParseTuple(args, "y#i", &serialized_options, &dummy, &buffer_len))
    {
        return NULL;
    }

    void * res = yami4_create_agent(serialized_options, buffer_len);

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_agent_terminate_incoming_queue(
    PyObject *self, PyObject *args)
{
    unsigned long long pa;
    if (!PyArg_ParseTuple(args, "K", &pa))
    {
        return NULL;
    }

    yami4_agent_terminate_incoming_queue(ull2ptr(pa));

    Py_RETURN_NONE;
}

static PyObject * yami4py_destroy_agent(PyObject *self, PyObject *args)
{
    unsigned long long pa;
    if (!PyArg_ParseTuple(args, "K", &pa))
    {
        return NULL;
    }

    yami4_destroy_agent(ull2ptr(pa));

    Py_RETURN_NONE;
}

static PyObject * yami4py_agent_add_listener(PyObject *self, PyObject *args)
{
    unsigned long long pa;
    const char * listener;
    int dummy;
    if (!PyArg_ParseTuple(args, "Ky#", &pa, &listener, &dummy))
    {
        return NULL;
    }

    void * res = yami4_agent_add_listener(ull2ptr(pa), listener);

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_agent_remove_listener(
    PyObject *self, PyObject *args)
{
    unsigned long long pa;
    const char * listener;
    int dummy;
    if (!PyArg_ParseTuple(args, "Ky#", &pa, &listener, &dummy))
    {
        return NULL;
    }

    yami4_agent_remove_listener(ull2ptr(pa), listener);

    Py_RETURN_NONE;
}

static PyObject * yami4py_agent_open_connection(
    PyObject *self, PyObject *args)
{
    unsigned long long pa;
    const char * target;
    int dummy;
    if (!PyArg_ParseTuple(args, "Ky#", &pa, &target, &dummy))
    {
        return NULL;
    }

    void * res = yami4_agent_open_connection(ull2ptr(pa), target);

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_agent_send(
    PyObject *self, PyObject *args)
{
    unsigned long long pa;
    const char * target;
    const char * object_name;
    const char * message_name;
    const char * serialized_content;
    int dummy;
    int buffer_len;
    int priority;
    int auto_connect;
    if (!PyArg_ParseTuple(args, "Ky#y#y#y#iii", &pa,
            &target, &dummy, &object_name, &dummy, &message_name, &dummy,
            &serialized_content, &dummy, &buffer_len,
            &priority, &auto_connect))
    {
        return NULL;
    }

    void * res;
    Py_BEGIN_ALLOW_THREADS

    res = yami4_agent_send(ull2ptr(pa), target,
        object_name, message_name, serialized_content, buffer_len,
        priority, auto_connect);

    Py_END_ALLOW_THREADS;

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_agent_send_one_way(
    PyObject *self, PyObject *args)
{
    unsigned long long pa;
    const char * target;
    const char * object_name;
    const char * message_name;
    const char * serialized_content;
    int dummy;
    int buffer_len;
    int priority;
    int auto_connect;
    if (!PyArg_ParseTuple(args, "Ky#y#y#y#iii", &pa,
            &target, &dummy, &object_name, &dummy, &message_name, &dummy,
            &serialized_content, &dummy, &buffer_len,
            &priority, &auto_connect))
    {
        return NULL;
    }

    void * res;
    Py_BEGIN_ALLOW_THREADS

    res = yami4_agent_send_one_way(ull2ptr(pa), target,
        object_name, message_name, serialized_content, buffer_len,
        priority, auto_connect);

    Py_END_ALLOW_THREADS;

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_agent_get_next_incoming_message(
    PyObject *self, PyObject *args)
{
    unsigned long long pa;
    if (!PyArg_ParseTuple(args, "K", &pa))
    {
        return NULL;
    }

    void * res;
    Py_BEGIN_ALLOW_THREADS

    res = yami4_agent_get_next_incoming_message(ull2ptr(pa));

    Py_END_ALLOW_THREADS;

    if (res != NULL)
    {
        return Py_BuildValue("K", ptr2ull(res));
    }
    else
    {
        Py_RETURN_NONE;
    }
}

static PyObject * yami4py_agent_get_next_connection_event(
    PyObject *self, PyObject *args)
{
    unsigned long long pa;
    if (!PyArg_ParseTuple(args, "K", &pa))
    {
        return NULL;
    }

    void * res;
    Py_BEGIN_ALLOW_THREADS

    res = yami4_agent_get_next_connection_event(ull2ptr(pa));

    Py_END_ALLOW_THREADS

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_agent_close_connection(
    PyObject *self, PyObject *args)
{
    unsigned long long pa;
    const char * target;
    int dummy;
    int priority;
    if (!PyArg_ParseTuple(args, "Ky#i", &pa, &target, &dummy, &priority))
    {
        return NULL;
    }

    yami4_agent_close_connection(ull2ptr(pa), target, priority);

    Py_RETURN_NONE;
}

static PyObject * yami4py_destroy_outgoing_message(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    if (!PyArg_ParseTuple(args, "K", &pmsg))
    {
        return NULL;
    }

    yami4_destroy_outgoing_message(ull2ptr(pmsg));

    Py_RETURN_NONE;
}

static PyObject * yami4py_outgoing_message_get_state(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    if (!PyArg_ParseTuple(args, "K", &pmsg))
    {
        return NULL;
    }

    void * res = yami4_outgoing_message_get_state(ull2ptr(pmsg));

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_outgoing_message_wait_for_transmission(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    int timeout;
    if (!PyArg_ParseTuple(args, "Ki", &pmsg, &timeout))
    {
        return NULL;
    }

    void * res;
    Py_BEGIN_ALLOW_THREADS

    res = yami4_outgoing_message_wait_for_transmission(
        ull2ptr(pmsg), timeout);

    Py_END_ALLOW_THREADS

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_outgoing_message_wait_for_completion(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    int timeout;
    if (!PyArg_ParseTuple(args, "Ki", &pmsg, &timeout))
    {
        return NULL;
    }

    void * res;
    Py_BEGIN_ALLOW_THREADS

    res = yami4_outgoing_message_wait_for_completion(
        ull2ptr(pmsg), timeout);

    Py_END_ALLOW_THREADS

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_outgoing_message_get_raw_reply(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    if (!PyArg_ParseTuple(args, "K", &pmsg))
    {
        return NULL;
    }

    void * res = yami4_outgoing_message_get_raw_reply(ull2ptr(pmsg));

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_outgoing_message_get_exception_msg(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    if (!PyArg_ParseTuple(args, "K", &pmsg))
    {
        return NULL;
    }

    void * res = yami4_outgoing_message_get_exception_msg(ull2ptr(pmsg));

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_incoming_message_get_source(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    if (!PyArg_ParseTuple(args, "K", &pmsg))
    {
        return NULL;
    }

    void * res = yami4_incoming_message_get_source(ull2ptr(pmsg));

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_incoming_message_get_object_name(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    if (!PyArg_ParseTuple(args, "K", &pmsg))
    {
        return NULL;
    }

    void * res = yami4_incoming_message_get_object_name(ull2ptr(pmsg));

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_incoming_message_get_message_name(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    if (!PyArg_ParseTuple(args, "K", &pmsg))
    {
        return NULL;
    }

    void * res = yami4_incoming_message_get_message_name(ull2ptr(pmsg));

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_incoming_message_get_raw_content(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    if (!PyArg_ParseTuple(args, "K", &pmsg))
    {
        return NULL;
    }

    void * res = yami4_incoming_message_get_raw_content(ull2ptr(pmsg));

    return Py_BuildValue("K", ptr2ull(res));
}

static PyObject * yami4py_incoming_message_reply(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    const char * serialized_content;
    int dummy;
    int buffer_len;
    int priority;
    if (!PyArg_ParseTuple(args, "Ky#ii", &pmsg,
            &serialized_content, &dummy, &buffer_len, &priority))
    {
        return NULL;
    }

    yami4_incoming_message_reply(ull2ptr(pmsg),
        serialized_content, buffer_len, priority);

    Py_RETURN_NONE;
}

static PyObject * yami4py_incoming_message_reject(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    const char * reason;
    int dummy;
    int priority;
    if (!PyArg_ParseTuple(args, "Ky#i", &pmsg, &reason, &dummy, &priority))
    {
        return NULL;
    }

    yami4_incoming_message_reject(ull2ptr(pmsg), reason, priority);

    Py_RETURN_NONE;
}

static PyObject * yami4py_destroy_incoming_message(
    PyObject *self, PyObject *args)
{
    unsigned long long pmsg;
    if (!PyArg_ParseTuple(args, "K", &pmsg))
    {
        return NULL;
    }

    yami4_destroy_incoming_message(ull2ptr(pmsg));

    Py_RETURN_NONE;
}

static PyObject * yami4py_read_from_binary_array(
    PyObject *self, PyObject *args)
{
    unsigned long long parray;
    int index;
    if (!PyArg_ParseTuple(args, "Ki", &parray, &index))
    {
        return NULL;
    }

    signed char res = *(static_cast<signed char *>(ull2ptr(parray)) + index);

    return Py_BuildValue("b", res);
}

static PyMethodDef yami4pyMethods[] = {
    {"yami4_is_success", yami4py_is_success, METH_VARARGS, ""},
    {"yami4_get_pointer", yami4py_get_pointer, METH_VARARGS, ""},
    {"yami4_get_int_i", yami4py_get_int_i, METH_VARARGS, ""},
    {"yami4_get_int_j", yami4py_get_int_j, METH_VARARGS, ""},
    {"yami4_get_int_k", yami4py_get_int_k, METH_VARARGS, ""},
    {"yami4_get_string", yami4py_get_string, METH_VARARGS, ""},
    {"yami4_get_error", yami4py_get_error, METH_VARARGS, ""},
    {"yami4_destroy_result", yami4py_destroy_result, METH_VARARGS, ""},
    {"yami4_create_agent", yami4py_create_agent, METH_VARARGS, ""},
    {"yami4_agent_terminate_incoming_queue",
     yami4py_agent_terminate_incoming_queue, METH_VARARGS, ""},
    {"yami4_destroy_agent", yami4py_destroy_agent, METH_VARARGS, ""},
    {"yami4_agent_add_listener",
     yami4py_agent_add_listener, METH_VARARGS, ""},
    {"yami4_agent_remove_listener",
     yami4py_agent_remove_listener, METH_VARARGS, ""},
    {"yami4_agent_open_connection",
     yami4py_agent_open_connection, METH_VARARGS, ""},
    {"yami4_agent_send", yami4py_agent_send, METH_VARARGS, ""},
    {"yami4_agent_send_one_way",
     yami4py_agent_send_one_way, METH_VARARGS, ""},
    {"yami4_agent_get_next_incoming_message",
     yami4py_agent_get_next_incoming_message, METH_VARARGS, ""},
    {"yami4_agent_get_next_connection_event",
     yami4py_agent_get_next_connection_event, METH_VARARGS, ""},
    {"yami4_agent_close_connection",
     yami4py_agent_close_connection, METH_VARARGS, ""},
    {"yami4_destroy_outgoing_message",
     yami4py_destroy_outgoing_message, METH_VARARGS, ""},
    {"yami4_outgoing_message_get_state",
     yami4py_outgoing_message_get_state, METH_VARARGS, ""},
    {"yami4_outgoing_message_wait_for_transmission",
     yami4py_outgoing_message_wait_for_transmission, METH_VARARGS, ""},
    {"yami4_outgoing_message_wait_for_completion",
     yami4py_outgoing_message_wait_for_completion, METH_VARARGS, ""},
    {"yami4_outgoing_message_get_raw_reply",
     yami4py_outgoing_message_get_raw_reply, METH_VARARGS, ""},
    {"yami4_outgoing_message_get_exception_msg",
     yami4py_outgoing_message_get_exception_msg, METH_VARARGS, ""},
    {"yami4_incoming_message_get_source",
     yami4py_incoming_message_get_source, METH_VARARGS, ""},
    {"yami4_incoming_message_get_object_name",
     yami4py_incoming_message_get_object_name, METH_VARARGS, ""},
    {"yami4_incoming_message_get_message_name",
     yami4py_incoming_message_get_message_name, METH_VARARGS, ""},
    {"yami4_incoming_message_get_raw_content",
     yami4py_incoming_message_get_raw_content, METH_VARARGS, ""},
    {"yami4_incoming_message_reply",
     yami4py_incoming_message_reply, METH_VARARGS, ""},
    {"yami4_incoming_message_reject",
     yami4py_incoming_message_reject, METH_VARARGS, ""},
    {"yami4_destroy_incoming_message",
     yami4py_destroy_incoming_message, METH_VARARGS, ""},
    {"yami4_read_from_binary_array",
     yami4py_read_from_binary_array, METH_VARARGS, ""},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef yami4pymodule = {
   PyModuleDef_HEAD_INIT, "yami4py", NULL, -1, yami4pyMethods
};

extern "C" PyMODINIT_FUNC PyInit_yami4py(void)
{
    return PyModule_Create(&yami4pymodule);
}
