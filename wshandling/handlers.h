#ifndef HANDLERS_H
#define	HANDLERS_H
#include "handler.h"
#include "ios_wrapper.h"
#include <memory>
#include <mutex>

namespace home_system
{

class handlers;

typedef std::unique_ptr<handlers> handlers_t;

class handlers {
public:
  handlers(const handlers& orig) = delete;
  ~handlers();
  
  static handlers_t create()
  {
    return handlers_t(new handlers());
  };
  
  void add(handler_t handler);
  void remove(handler_t handler);
  
private:
  handlers();
  
  ios_wrapper ios_;
  std::mutex mut_;
  
  void select();
  void read(handler_t handler);
  void send(handler_t handler);
  
  typedef std::map<Poco::Net::WebSocket, handler_t> ws_to_handler_map_t;
  Poco::Net::Socket::SocketList list_;
  ws_to_handler_map_t ws_to_handler_map_;
};

}

extern home_system::handlers_t _handlers;

#define HANDLERS (*::_handlers)

#endif	/* WS_HANDLER_H */

