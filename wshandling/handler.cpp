#include "handler.h"
#include "handlers.h"
#include "logger.h"
#include <exception>

using namespace Poco;
using namespace Poco::Net;
using namespace std;

namespace home_system
{

data_t create_data()
{
  return data_t(new std::array<char, MAX_DATA_SIZE>());
}

handler::handler(ws_t ws, bool use_idle_ping)
: state_(state::created),
  ws_(ws),
  use_idle_ping_(use_idle_ping)
{
  lock_guard<mutex> lock(state_mutex_);
  LOG(DEBUG) << "Handler creating";
}

void handler::set_up_timer()
{
  if (use_idle_ping_)
  {
    timer_.cancel();
    timer_.set_from_now(15000, [this] ()
    {
      // just to keep WebSocket busy
      LOG(DEBUG) << "Idle for more than 15 seconds, sending ping message";
      buffer_t buffer(new rapidjson::StringBuffer);
      
      buffer->Put('p');
      buffer->Put('i');
      buffer->Put('n');
      buffer->Put('g');
      buffer->Put(0);

      // sending to cloud server
      on_send(shared_from_this(), buffer);
    });
  }
}

void handler::init()
{
  lock_guard<mutex> lock(state_mutex_);
  if (state_ == state::created)
  {
    LOG(DEBUG) << "Handler initializing";
    ws_->setBlocking(false);
    HANDLERS.add(shared_from_this());
    LOG(DEBUG) << "Handler initialized";
    state_ = state::initialized;
    set_up_timer();
  }
}

handler::~handler()
{
  this->shutdown();
  LOG(DEBUG) << "Handler destroyed";
}

Poco::Net::WebSocket handler::ws()
{
  return *ws_;
}

size_t handler::read(data_t data)
{
  lock_guard<mutex> lock(state_mutex_);
  if (state_ == state::initialized)
  {
    return read_internal(data);
  }
  else
  {
    throw runtime_error("read on not initialized handler");
  }
}

size_t handler::read_internal(data_t data)
{
  int flags;
  size_t n = ws_->receiveFrame((*data).data(), DATA_SIZE, flags);

  //LOG(TRACE) << "Received " << n << " bytes with " << flags << " flags, message: " << string((*data).data(), n);

  if (n == 0)
  {
    throw runtime_error("Peer shut down or closed connection");
  }
  
  set_up_timer();

  switch (flags & WebSocket::FRAME_OP_BITMASK)
  {
    case WebSocket::FRAME_OP_TEXT:
      // frames which are to be processed
      // checking for ping which is used only to keep WebSocket connection busy
      if (n == 5)
      {
        (*data)[4] = '\0';
        if (strcmp((*data).data(), "ping") == 0)
        {
          n = 0;
        }
      }
      break;
    case WebSocket::FRAME_OP_CONT:
    case WebSocket::FRAME_OP_PONG:
    case WebSocket::FRAME_OP_PING:
    case WebSocket::FRAME_OP_BINARY:
      // ignore
      n = 0;
      break;
    case WebSocket::FRAME_OP_CLOSE:
      throw runtime_error("WebSocket close request received");
  }
  return n;
}

void handler::on_send(handler_t handler, data_t data, size_t data_size)
{
  // posts send request to handlers WebSocket handling thread
  HANDLERS.post_send(handler, data, data_size);
}

void handler::on_send(handler_t handler, buffer_t buffer)
{
  // posts send request to handlers WebSocket handling thread
  HANDLERS.post_send(handler, buffer);
}

void handler::send(data_t data, size_t data_size)
{
  lock_guard<mutex> lock(state_mutex_);
  if (state_ == state::initialized)
  {
    send_internal(data, data_size);
  }
}

void handler::send(buffer_t buffer)
{
  lock_guard<mutex> lock(state_mutex_);
  if (state_ == state::initialized)
  {
    send_internal(buffer->GetString(), buffer->GetSize());
  }
}

void handler::send_internal(data_t data, size_t data_size)
{
  send_internal((*data).data(), data_size);
}

void handler::send_internal(const void* data, size_t data_size)
{
  set_up_timer();
  //LOG(TRACE) << "Sending " << data_size << " bytes, message: " << string((const char*)data, data_size);
  ws_->sendFrame(data, data_size);
}

void handler::shutdown()
{
  lock_guard<mutex> lock(state_mutex_);
  if (state_ == state::initialized)
  {
    timer_.cancel();
    try
    {
      state_ = state::shutdown;
      LOG(DEBUG) << "Handler shutdown";
      ws_->shutdown();
    }
    catch (const exception& e)
    {
      LOG(ERROR) << e.what();
    }
  }
}

}
