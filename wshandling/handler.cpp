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

handler::handler(ws_t ws)
: state_(state::created),
  ws_(ws)
{
  LOG("Handler creating");
}

void handler::init()
{
  if (state_ == state::created)
  {
    LOG("Handler initializing");
    //ws_->setBlocking(false);
    HANDLERS.add(shared_from_this());
    LOG("Handler initialized");
    state_ = state::initialized;
  }
}

handler::~handler()
{
  shutdown();
  LOG("Handler destroyed");
}

Poco::Net::WebSocket handler::ws()
{
  return *ws_;
}

size_t handler::read(data_t data)
{
  if (state_ == state::initialized)
  {
    int flags;
    size_t n = ws_->receiveFrame((*data).data(), DATA_SIZE, flags);
    
    //LOG("Received " << n << " bytes with " << flags << " flags");

    if (n == 0)
    {
      throw runtime_error("Peer shut down or closed connection");
    }
    
    switch (flags & WebSocket::FRAME_OP_BITMASK)
    {
      case WebSocket::FRAME_OP_TEXT:
        // frames which are to be processed
        break;
      case WebSocket::FRAME_OP_CONT:
      case WebSocket::FRAME_OP_PONG:
      case WebSocket::FRAME_OP_BINARY:
        // ignore
        n = 0;
        break;
      case WebSocket::FRAME_OP_PING:
        ws_->sendFrame(nullptr, 0, WebSocket::FRAME_OP_PONG);
        break;
      case WebSocket::FRAME_OP_CLOSE:
        throw runtime_error("WebSocket close request received");
    }
    return n;
  }
  else
  {
    throw runtime_error("read on not initialized handler");
  }
}

void handler::on_send(handler_t handler, data_t data, size_t data_size)
{
  if (handler->state_ == state::initialized)
  {
    // posts send request to handlers WebSocket handling thread
    HANDLERS.post_send(handler, data, data_size);
  }
}

void handler::send(data_t data, size_t data_size)
{
  if (state_ == state::initialized)
  {
    ws_->sendFrame((*data).data(), data_size);
  }
}

void handler::shutdown()
{
  if (state_ == state::initialized)
  {
    try
    {
      state_ = state::shutdown;
      LOG("Handler shutdown");
      ws_->shutdown();
    }
    catch (const exception& e)
    {
      LOGERROR(e.what());
    }
  }
}

}
