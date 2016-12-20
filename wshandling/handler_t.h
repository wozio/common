#ifndef HANDLER_T_H
#define HANDLER_T_H

#include <Poco/Net/WebSocket.h>
#include <memory>

namespace home_system
{

  class handler;
  typedef std::shared_ptr<handler> handler_t;

  typedef std::shared_ptr<Poco::Net::WebSocket> ws_t;

}

#endif /* HANDLER_T_H */