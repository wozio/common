#ifndef HANDLER_H
#define	HANDLER_H

#include <Poco/Net/WebSocket.h>
#include <memory>

namespace home_system
{

typedef std::shared_ptr<Poco::Net::WebSocket> ws_t;
typedef std::shared_ptr<std::array<char, 1024>> data_t;
data_t create_data();

class handler;
typedef std::shared_ptr<handler> handler_t;

class handler
: public std::enable_shared_from_this<handler>
{
public:
  handler(ws_t ws);
  handler(const handler& orig) = delete;
  virtual ~handler();
  
  Poco::Net::WebSocket ws();
  size_t read(data_t data);
  void send(data_t data, size_t data_size);
  
  virtual void on_read(data_t data, size_t data_size) = 0;
  static void on_send(handler_t handler, data_t data, size_t data_size);
  
  virtual void shutdown() = 0;
  virtual std::string name() = 0;
  
private:
  ws_t ws_;
};

}

#endif	/* HANDLER_H */

