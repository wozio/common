#include "yamicontainer.h"
#include <Poco/Net/NetworkInterface.h>
#include <sstream>
#include <iostream>

#define LOG(x) if (log_callback_ != nullptr) {std::stringstream s; s << x; log_callback_(s.str());}

namespace home_system
{

yami_container::event_callback_impl::event_callback_impl(log_callback_t log_callback)
: log_callback_(log_callback)
{
}

yami_container::event_callback_impl::~event_callback_impl()
{
  log_callback_ = nullptr;
}

void yami_container::event_callback_impl::incoming_connection_open(const char * target)
{
  LOG("incoming_connection_open: " << target);
}

void yami_container::event_callback_impl::outgoing_connection_open(const char * target)
{
  LOG("outgoing_connection_open: " << target);
}

void yami_container::event_callback_impl::connection_closed(const char * target)
{
  LOG("connection_closed: " << target);
}

void yami_container::event_callback_impl::connection_error(const char * target)
{
  LOG("connection_error: " << target);
}

void yami_container::operator()(int ec, const char* desc)
{
  LOG("YAMI IO error: " << ec << " " << desc);
}

yami_container::yami_container(log_callback_t log_callback)
: ec_(log_callback),
  log_callback_(log_callback),
  agent_(ec_)
{
  agent_.register_io_error_logger(*this);
  
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
}

yami_container::~yami_container()
{
  log_callback_ = nullptr;
}

}
