#ifndef YAMICONTAINER_H
#define	YAMICONTAINER_H

#include <yami4-cpp/yami.h>
#include <memory>
#include <functional>
#include <string>
#include <map>
#include <mutex>

namespace home_system
{

class yami_container;

typedef std::unique_ptr<home_system::yami_container> yc_t;

class yami_container
{
public:

  static yc_t create()
  {
    return yc_t(new yami_container());
  }

  yami_container();
  ~yami_container();
  
  yami::agent& agent()
  {
    return agent_;
  }
  
  const std::string& endpoint()
  {
    return endpoint_;
  }

  void operator()(yami::incoming_message & im);

  typedef std::function<void(yami::incoming_message& im)> handler_t;
  void register_handler(const std::string& name, handler_t handler);
  void unregister_handler(const std::string& name);


private:
  yami::agent agent_;
  std::string endpoint_;
  std::mutex handlers_mutex_;
  std::map<std::string, handler_t> handlers_;
};

}

extern home_system::yc_t _yc;

#define YC (*::_yc)
#define AGENT YC.agent()

#endif	/* YAMICONTAINER_H */

