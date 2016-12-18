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
  LOGH(DEBUG) << "Handler creating";
}

void handler::set_up_timer()
{
  if (use_idle_ping_)
  {
    timer_.set_from_now(5000, [this] ()
    {
      if (!active_)
      {
        // just to keep WebSocket busy
        LOGH(DEBUG) << "Idle for too long, sending ping message";
        buffer_t buffer(new rapidjson::StringBuffer);

        buffer->Put('p');
        buffer->Put('i');
        buffer->Put('n');
        buffer->Put('g');
        buffer->Put(0);

        // sending to peer
        on_send(buffer);
      }
      set_up_timer();
    });
    active_ = false;
  }
}

void handler::init()
{
  lock_guard<mutex> lock(state_mutex_);
  if (state_ == state::created)
  {
    LOGH(DEBUG) << "Handler initializing";
    //ws_->setBlocking(false);
    HANDLERS.add(shared_from_this());
    LOGH(DEBUG) << "Handler initialized";
    state_ = state::initialized;
    set_up_timer();
  }
}

handler::~handler()
{
  this->shutdown();
  LOGH(DEBUG) << "Handler destroyed";
}

Poco::Net::WebSocket handler::ws()
{
  return *ws_;
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
      LOGH(DEBUG) << "Handler shutdown";
      ws_->shutdown();
    }
    catch (const exception& e)
    {
      LOGH(ERROR) << e.what();
    }
  }
}

void handler::on_read(data_t data, size_t data_size, type_t data_type)
{
  // ignore data
}

size_t handler::read(data_t data, type_t& data_type)
{
  lock_guard<mutex> lock(state_mutex_);
  if (state_ == state::initialized)
  {
    return read_internal(data, data_type);
  }
  else
  {
    throw runtime_error("read on not initialized handler");
  }
}

size_t handler::read_internal(data_t data, type_t& data_type)
{
  //LOGH(TRACE) << "Reading data";
  int flags = 0;
  size_t n = ws_->receiveFrame((*data).data(), DATA_SIZE, flags);

  //LOGH(TRACE) << "Received " <<n << " bytes with " << flags << " flags";

  if (n == 0)
  {
    throw runtime_error("Peer shut down or closed connection");
  }
  
  active_ = true;

  switch (flags & WebSocket::FRAME_OP_BITMASK)
  {
    case WebSocket::FRAME_OP_TEXT:
    case WebSocket::FRAME_OP_BINARY:
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
    //case WebSocket::FRAME_OP_CONT:
    case WebSocket::FRAME_OP_PONG:
    case WebSocket::FRAME_OP_PING:
      // ignore
      n = 0;
      break;
    case WebSocket::FRAME_OP_CLOSE:
      throw runtime_error("WebSocket close request received");
  }
  if ((flags & WebSocket::FRAME_OP_BITMASK) == WebSocket::FRAME_OP_BINARY)
  {
    data_type = BINARY;
  }
  else
  {
    data_type = TEXT;
  }
  return n;
}

void handler::on_send(data_t data, size_t data_size, type_t data_type)
{
  lock_guard<mutex> lock(state_mutex_);
  queue_item item(data, data_size, data_type);
  queue_.push_back(item);
}

void handler::on_send(buffer_t buffer, type_t data_type)
{
  lock_guard<mutex> lock(state_mutex_);
  queue_item item(buffer, data_type);
  queue_.push_back(item);
  //LOGH(TRACE) << "Queue size: " << queue_.size();
}

bool handler::something_to_send()
{
  lock_guard<mutex> lock(state_mutex_);
  return queue_.size() > 0;
}

void handler::send()
{
  lock_guard<mutex> lock(state_mutex_);
  if (state_ == state::initialized)
  {
    if (queue_.size() > 0)
    {
      active_ = true;
      queue_item item = queue_.front();
      //LOGH(TRACE) << "Sending " << item.size() << " bytes, queue size: " << queue_.size();
      try
      {
        item.send(ws_);
        queue_.pop_front();
      }
      catch (const Poco::Exception& e)
      {
        LOGH(TRACE) << e.displayText();
      }
    }
  }
}

handler::queue_item::queue_item(buffer_t buffer, type_t data_type)
  : buffer_(buffer),
  data_size_(0)
{
  data_type == TEXT ? send_flags_ = Poco::Net::WebSocket::FRAME_TEXT : send_flags_ = Poco::Net::WebSocket::FRAME_BINARY;
}

handler::queue_item::queue_item(data_t data, size_t data_size, type_t data_type)
  : data_(data),
  data_size_(data_size)
{
  data_type == TEXT ? send_flags_ = Poco::Net::WebSocket::FRAME_TEXT : send_flags_ = Poco::Net::WebSocket::FRAME_BINARY;
}

int handler::queue_item::send(ws_t ws)
{
  if (data_size_ > 0)
  {
    return ws->sendFrame(data_->data(), data_size_, send_flags_);
  }
  else
  {
    return ws->sendFrame(buffer_->GetString(), buffer_->GetSize(), send_flags_);
  }
}
size_t handler::queue_item::size()
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

}
