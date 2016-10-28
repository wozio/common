#include "client_binary_session.h"
#include <Poco/Net/NetworkInterface.h>

namespace home_system
{
  client_binary_session::client_binary_session()
  {
    // getting usable IP address
    Poco::Net::NetworkInterface::NetworkInterfaceList il = Poco::Net::NetworkInterface::list();
    std::string ip;
    for (size_t i = 0; i < il.size(); ++i)
    {
      if (!il[i].address().isLoopback())
        if (il[i].address().family() == Poco::Net::IPAddress::Family::IPv4)
          ip = il[i].address().toString();
    }

    agent_.reset(new yami::agent([]()->yami::parameters{
      yami::parameters p;
      p.set_boolean("deliver_as_raw_binary", true);
      return p;
    }()));

    // constructing endpoint for listener
    std::string ep("tcp://");
    ep.append(ip).append(":*");
    endpoint_ = agent_->add_listener(ep);

    agent_->register_object("*", *this);
  }

  client_binary_session::~client_binary_session()
  {
    agent_->unregister_object("*");
  }

  std::string client_binary_session::get_endpoint()
  {
    return endpoint_;
  }

  void client_binary_session::operator()(yami::incoming_message & im)
  {
    try
    {
      auto indata = im.get_raw_content();
      data_(indata);
    }
    catch (const std::exception& e)
    {
    }
  }
}