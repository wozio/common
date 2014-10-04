#include "yamicontainer.h"
#include <boost/asio.hpp>
#include <sstream>

#define LOG(x) if (log_callback_ != nullptr) {std::stringstream s; s << x; log_callback_(s.str());}

using boost::asio::ip::tcp;

namespace home_system
{

yami_container::event_callback_impl::event_callback_impl(log_callback_t log_callback)
: log_callback_(log_callback)
{
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
  
  endpoint_ = agent_.add_listener("tcp://*.*");
  
  std::string port = endpoint_.substr(endpoint_.find_last_of(":") + 1);
  
  size_t ind = endpoint_.find_last_of('/') + 1;
  size_t num = endpoint_.find_last_of(":") - ind;
  std::string hostname = endpoint_.substr(ind, num);
  
  boost::asio::io_service io_service;
  tcp::resolver resolver(io_service);
  tcp::resolver::query query(hostname, "");
  tcp::resolver::iterator iter = resolver.resolve(query);
  tcp::resolver::iterator end; // End marker.
  while (iter != end)
  {
      tcp::endpoint ep = *iter++;
      std::ostringstream s;
      s << "tcp://" << ep.address() << ":" << port;
      endpoint_ = s.str();
      break;
  }
}

yami_container::~yami_container()
{
  log_callback_ = nullptr;
}

}


