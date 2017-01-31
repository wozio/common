#include "yamicontainer.h"
#include <Poco/Net/NetworkInterface.h>
#include <sstream>
#include <iostream>

namespace home_system
{

yami::parameters agent_params()
{
  yami::parameters params;
  params.set_integer("tcp_connect_timeout", 150);
  return params;
}

yami_container::yami_container()
: agent_(agent_params())
{
  Poco::Net::NetworkInterface::NetworkInterfaceList il = Poco::Net::NetworkInterface::list();
  std::string ip;
  for (size_t i = 0; i < il.size(); ++i)
  {
    if (!il[i].address().isLoopback())
        if (il[i].address().family() == Poco::Net::IPAddress::Family::IPv4)
            ip = il[i].address().toString();
  }
  
  std::string ep("tcp://");
  ep.append(ip).append(":*");
  endpoint_ = agent_.add_listener(ep);

  agent_.register_object("*", *this);
}

yami_container::~yami_container()
{
  agent_.unregister_object("*");
  std::lock_guard<std::mutex> guard(handlers_mutex_);
  handlers_.clear();
}

void yami_container::register_handler(const std::string& name, std::function<void(yami::incoming_message& im)> handler)
{
  std::lock_guard<std::mutex> guard(handlers_mutex_);
  handlers_[name] = handler;
}

void yami_container::unregister_handler(const std::string& name)
{
  std::lock_guard<std::mutex> guard(handlers_mutex_);
  handlers_.erase(name);
}

void yami_container::operator()(yami::incoming_message & im)
{
  std::lock_guard<std::mutex> guard(handlers_mutex_);
  auto hi = handlers_.find(im.get_object_name());
  if (hi != handlers_.end())
  {
    try
    {
      hi->second(im);
    }
    catch (const std::exception& e)
    {
      //.....
    }
  }
}

}
