#ifndef HANDLER_H
#define	HANDLER_H

#include "timer.h"
#include "handler_t.h"
#include "rapidjson/stringbuffer.h"
#include <Poco/Net/WebSocket.h>
#include <memory>
#include <array>
#include <mutex>

namespace home_system
{
  
#define DATA_SIZE 1024
#define MAX_DATA_SIZE 1025

typedef std::shared_ptr<Poco::Net::WebSocket> ws_t;
// it is bigger by one to ensure that we always have place to put '\0' character
// at the end
typedef std::shared_ptr<std::array<char, MAX_DATA_SIZE>> data_t;
typedef std::shared_ptr<rapidjson::StringBuffer> buffer_t;

data_t create_data();

class handler
: public std::enable_shared_from_this<handler>
{
public:
  handler(ws_t ws, bool use_idle_ping);
  handler(const handler& orig) = delete;
  virtual ~handler();
  
  Poco::Net::WebSocket ws();
  
  /**
   * Read data from underlying WebSocket into supplied data buffer.
   * @param data Data buffer
   * @return Number of read bytes
   */
  size_t read(data_t data);
  
  /**
   * Send provided data to underlying WebSocket.
   * Derived classes may override this method to additionally process data
   * before sending.
   * Any exception thrown from this method will lead to removing this handler.
   * Call this method from this base class to send data.
   * @param data Data buffer to send
   * @param data_size Size of the data to send
   */
  virtual void send(data_t data, size_t data_size);
  
  /**
   * Send provided data to underlying WebSocket.
   * Derived classes may override this method to additionally process data
   * before sending.
   * Any exception thrown from this method will lead to removing this handler.
   * Call this method from this base class to send data.
   * @param buffer Data buffer to send
   */
  virtual void send(buffer_t buffer);
  
  /**
   * Handle incoming data.
   * Called when data has been successfully read from WebSocket.
   * Derived classes must override this method to handle incoming data.
   * Any exception thrown from this method will lead to removing this handler.
   * @param data Data buffer
   * @param data_size Data size
   */
  virtual void on_read(data_t data, size_t data_size) = 0;
  
  /**
   * Request sending of data.
   * @param handler Handler which should send the data
   * @param data Data to send
   * @param data_size Data size
   */
  static void on_send(handler_t handler, data_t data, size_t data_size);
  
  /**
   * Request sending of data.
   * @param handler Handler which should send the data
   * @param buffer Buffer of data to send
   */
  static void on_send(handler_t handler, buffer_t buffer);
  
  void init();
  virtual void shutdown();
  
protected:
  size_t read_internal(data_t data);
  void send_internal(data_t data, size_t data_size);
  void send_internal(const void* data, size_t data_size);

private:
  
  enum class state
  {
    created,
    initialized,
    shutdown
  } state_;
  
  std::mutex state_mutex_;
  
  ws_t ws_;
  bool use_idle_ping_;
  
  timer timer_;
  void set_up_timer();
};

}

#endif	/* HANDLER_H */

