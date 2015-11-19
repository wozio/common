#include "handlers.h"
#include "logger.h"
#include <utility>
#include <chrono>

using namespace std;
using namespace std::chrono;
using namespace Poco;
using namespace Poco::Net;

namespace home_system
{

handlers::handlers()
{
  ios_.io_service().post([this] ()
  {
    this->select();
  });
}

handlers::~handlers()
{
  
}

void handlers::add(handler_t handler)
{
  ws_to_handler_map_[handler->ws()] = handler;
  list_.push_back(handler->ws());
}

void handlers::remove(handler_t handler)
{
  for (Socket::SocketList::iterator i = list_.begin(); i != list_.end(); ++i)
  {
    if (*i == handler->ws())
    {
      list_.erase(i);
      break;
    }
  }
  ws_to_handler_map_.erase(handler->ws());
}

void handlers::select()
{
  //LOG("select");
  Socket::SocketList readList(list_);
  Socket::SocketList writeList;
  Socket::SocketList exceptList;
  
  int n = Socket::select(readList, writeList, exceptList, Timespan(0, 1000));
  
  //LOG("select: " << n);
  
  if (n > 0)
  {
    for (auto socket : readList)
    {
      auto handler = ws_to_handler_map_[socket];
      // start reading from assiociated websocket
      ios_.io_service().post([this, handler] ()
        {
          this->read(handler);
        }
      );
    }
  }
  else
  {
    this_thread::sleep_for(milliseconds(1));
  }
  ios_.io_service().post([this] ()
  {
    this->select();
  });
  //LOG("after select");
}

void handlers::read(handler_t handler)
{
  try
  {
    auto data = create_data();
    int n = handler->read(data);
    if (n > 0)
    {
      handler->on_read(data, n);
    }
  }
  catch (...)
  {
    // anything thrown from handler's read leads to deleting handler
    handler->shutdown();
  }
}

void handlers::post_send(handler_t handler, data_t data, size_t data_size)
{
  ios_.io_service().post([this, handler, data, data_size] ()
    {
      this->send(handler, data, data_size);
    }
  );
}

void handlers::send(handler_t handler, data_t data, int data_size)
{
  try
  {
    handler->send(data, data_size);
  }
  catch (...)
  {
    // anything thrown from handler's send leads to deleting handler
    handler->shutdown();
  }
}

}
