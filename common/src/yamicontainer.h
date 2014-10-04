#ifndef YAMICONTAINER_H
#define	YAMICONTAINER_H

#include <yami4-cpp/yami.h>
#include <memory>
#include <functional>

namespace home_system
{

class yami_container;

typedef std::unique_ptr<home_system::yami_container> yc_t;

class yami_container
{
public:

  typedef std::function<void(const std::string&)> log_callback_t;
  static yc_t create(log_callback_t log_callback = nullptr)
  {
    return yc_t(new yami_container(log_callback));
  }

  yami_container(log_callback_t log_callback);
  ~yami_container();
  
  class event_callback_impl : public yami::event_callback
  {
  public:
    event_callback_impl(log_callback_t log_callback);

  private:
    void incoming_connection_open(const char * target);
    void outgoing_connection_open(const char * target);
    void connection_closed(const char * target);
    void connection_error(const char * target);

    log_callback_t log_callback_;
  } ec_;
  
  yami::agent& agent()
  {
    return agent_;
  }
  
  const std::string& endpoint()
  {
    return endpoint_;
  }

  void operator()(int ec, const char* desc);

private:
  log_callback_t log_callback_;
  yami::agent agent_;
  std::string endpoint_;
};

}

extern home_system::yc_t _yc;

#define YC (*::_yc)
#define AGENT YC.agent()

#endif	/* YAMICONTAINER_H */

