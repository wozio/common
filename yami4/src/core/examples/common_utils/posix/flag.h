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
