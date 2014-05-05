#ifndef YAMICONTAINER_H
#define	YAMICONTAINER_H

#include <yami4-cpp/yami.h>

namespace home_system
{

class yami_container
{
public:
  yami_container();
  ~yami_container(){};
  
  class event_callback_impl : public yami::event_callback
  {
    void incoming_connection_open(const char * target);
    void outgoing_connection_open(const char * target);
    void connection_closed(const char * target);
    void connection_error(const char * target);
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
  yami::agent agent_;
  std::string endpoint_;
};

}

extern home_system::yami_container _yc;

#define YC ::_yc
#define AGENT YC.agent()

#endif	/* YAMICONTAINER_H */

