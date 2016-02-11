#ifndef HANDLER_H
#define	HANDLER_H

#include <Poco/Net/WebSocket.h>
#include <memory>
#include <array>

namespace home_system
{
  
#define DATA_SIZE 1024
#define MAX_DATA_SIZE 1025

typedef std::shared_ptr<Poco::Net::WebSocket> ws_t;
// it is bigger by one to ensure that we always have place to put '\0' character
// at the end
typedef std::shared_ptr<std::array<char, MAX_DATA_SIZE>> data_t;

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
  
  void init();
  virtual void shutdown();

private:
  enum class state
  {
    created,
    initialized,
    shutdown
  } state_;
  
  ws_t ws_;
};

}

#endif	/* HANDLER_H */

