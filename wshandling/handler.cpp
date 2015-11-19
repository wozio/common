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
  return data_t(new std::array<char, 1024>());
}

handler::handler(ws_t ws)
: ws_(ws)
{
  ws_->setBlocking(false);
  LOG("Handler created");
}

handler::~handler()
{
  LOG("Handler destroyed");
}

Poco::Net::WebSocket handler::ws()
{
  return *ws_;
}

size_t handler::read(data_t data)
{
  int flags;
  size_t n = ws_->receiveFrame((*data).data(), (*data).size(), flags);
  
  if ((flags & WebSocket::FRAME_OP_BITMASK) == WebSocket::FRAME_OP_CLOSE)
  {
    throw runtime_error("WebSocket close request received");
  }
  else if ((flags & WebSocket::FRAME_OP_BITMASK) != WebSocket::FRAME_OP_TEXT)
  {
    throw runtime_error("Only text frames are supported on WebSocket");
  }
  return n;
}

void handler::on_send(handler_t handler, data_t data, size_t data_size)
{
  // posts send request to hadlers WebSocket hadling thread
  HANDLERS.post_send(handler, data, data_size);
}

void handler::send(data_t data, size_t data_size)
{
  ws_->sendFrame((*data).data(), data_size);
}

void handler::shutdown()
{
  ws_->shutdown();
  LOG(this->name() << " Handler shut down");
}

}
