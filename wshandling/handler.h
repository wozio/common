#ifndef HANDLER_H
#define	HANDLER_H

#include "timer.h"
#include "handler_t.h"
#include "rapidjson/stringbuffer.h"
#include <Poco/Net/WebSocket.h>
#include <memory>
#include <array>
#include <mutex>

#define LOGH(level) LOG(level) << "(" << this << ") "

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
   * Send previously provided in on_send data to underlying WebSocket.
   * Derived classes may override this method to additionally process data
   * before sending.
   * Any exception thrown from this method will lead to removing this handler.
   * Call this method to actually send data.
   */
  virtual void send();
  
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
   * @param data Data to send
   * @param data_size Data size
   */
  void on_send(data_t data, size_t data_size);
  
  /**
   * Request sending of data.
   * @param buffer Buffer of data to send
   */
  void on_send(buffer_t buffer);
  
  void init();
  virtual void shutdown();
  bool something_to_send();
  
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
  
  class queue_item
  {
  public:
    queue_item(buffer_t buffer)
    : buffer_(buffer),
      data_size_(0)
    {
    }
    queue_item(data_t data, size_t data_size)
    : data_(data),
      data_size_(data_size)
    {
    }
    int send(ws_t ws)
    {
      if (data_size_ > 0)
      {
        return ws->sendFrame(data_->data(), data_size_, Poco::Net::WebSocket::FRAME_BINARY);
      }
      else
      {
        return ws->sendFrame(buffer_->GetString(), buffer_->GetSize(), Poco::Net::WebSocket::FRAME_BINARY);
      }
    }
    size_t size()
    {
      if (data_size_ > 0)
      {
        return data_size_;
      }
      else
      {
        return buffer_->GetSize();
      }
    }
  private:
    buffer_t buffer_;
    data_t data_;
    size_t data_size_;
  };
  std::list<queue_item> queue_;
};

}

#endif	/* HANDLER_H */

