#undef _WINSOCKAPI_
#include "service.h"
#include "mcs.h"
#include "discovery.h"
#include "logger.h"
#include "yamicontainer.h"
#include <sstream>
#include <fstream>
#include <string>

using namespace std;
using namespace boost;
using namespace boost::asio;

namespace home_system
{

service::service(const std::string& name, bool initialize)
: name_(name),
  notify_dt_(ios_.io_service()),
  initialize_(initialize)
{
  if (initialize_)
  {
    init();
  }
}

service::~service()
{
  if (initialize_)
  {
    deinit();
  }
}

void service::init()
{
  AGENT.register_object(name_, *this);
  
  set_notify_timeout();
  
#ifndef DISABLE_LOGS
  LOG(DEBUG) << "Initialized service with name: " << name_ << " and YAMI endpoint: " << ye();
#endif
  
  send_hello();
}

void service::deinit()
{
#ifndef DISABLE_LOGS
  LOG(DEBUG) << "Deinitialized service with name: " << name_;
#endif
  AGENT.unregister_object(name_);
  
  notify_dt_.cancel();
  
  send_bye();
}

std::string service::name() const
{
  return name_;
}

std::string service::ye() const
{
  return YC.endpoint();
}

void service::on_msg(yami::incoming_message & im)
{
#ifndef DISABLE_LOGS
  LOG(WARNING) << name_ << ": unknown message: " << im.get_message_name();
#endif
  im.reject("unknown message");
}

void service::operator()(yami::incoming_message & im)
{
#ifndef DISABLE_LOGS
  //LOG(TRACE) << "message " << im.get_message_name() << " from " << im.get_source();
#endif
  try
  {
    on_msg(im);
  }
  catch (const std::exception& e)
  {
#ifndef DISABLE_LOGS
    LOG(WARNING) << name_ << ": EXCEPTION: " << e.what();
#endif
    im.reject(e.what());
  }
}

// multicast send

void service::send_hello()
{
  ostringstream str;
  str << "hello\n"
    << name_ << "\n"
    << ye();
  if (extra_discovery_data_.size() > 0)
  {
    str << "\n" << extra_discovery_data_;
  }
  multicast_send(str.str());
}

void service::send_notify()
{
  ostringstream str;
  str << "notify\n"
    << name_ << "\n"
    << ye();
  if (extra_discovery_data_.size() > 0)
  {
    str << "\n" << extra_discovery_data_;
  }
  multicast_send(str.str());
  
  set_notify_timeout();
}

void service::set_notify_timeout()
{
  notify_dt_.cancel();
  notify_dt_.expires_from_now(posix_time::seconds(rand() % 4 + 1));
  notify_dt_.async_wait( [&] (const boost::system::error_code& error) { on_notify_timeout(error); } );
}

void service::send_bye()
{
  ostringstream str;
  str << "bye\n"
    << name_;
  multicast_send(str.str());
}

void service::on_notify_timeout(const boost::system::error_code& error)
{
  if (!error)
  {
    send_notify();
  }
}

}
