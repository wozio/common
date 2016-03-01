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
}

handlers::~handlers()
{
  ios_.stop_ios();

  LOG(DEBUG) << "Handlers destroing";
  
  list_.clear();
  ws_to_handler_map_.clear();
}

void handlers::add(handler_t handler)
{
  lock_guard<mutex> l(mut_);
  
  LOG(DEBUG) << "Handler add";
  
  ws_to_handler_map_[handler->ws()] = handler;
  list_.push_back(handler->ws());
  
  if (list_.size() == 1)
  {
    ios_.io_service().post([this] ()
    {
      this->select();
    });
  }
}

void handlers::remove(handler_t handler)
{
  lock_guard<mutex> l(mut_);
  
  LOG(DEBUG) << "Handler remove";
  
  handler->shutdown();
  
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
  if (list_.size() > 0)
  {
    Socket::SocketList readList(list_);
    Socket::SocketList writeList;
    Socket::SocketList exceptList;

    Socket::select(readList, writeList, exceptList, Timespan(0, 100000));
    
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
  
  lock_guard<mutex> l(mut_);
  
  if (list_.size() > 0)
  {
    ios_.io_service().post([this] ()
    {
      this->select();
    });
  }
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
  catch (const exception& e)
  {
    LOG(DEBUG) << "Exception on read '" << e.what() << "', removing handler";
    remove(handler);
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

void handlers::post_send(handler_t handler, rapidjson::StringBuffer&& buffer)
{
  ios_.io_service().post([this, handler, buffer] ()
    {
      this->send(handler, buffer);
    }
  );
}

void handlers::send(handler_t handler, data_t data, int data_size)
{
  try
  {
    handler->send(data, data_size);
  }
  catch (const exception& e)
  {
    LOG(DEBUG) << "Exception on send '" << e.what() << "', removing handler";
    remove(handler);
  }
}

void handlers::send(handler_t handler, rapidjson::StringBuffer&& buffer)
{
  try
  {
    handler->send(buffer);
  }
  catch (const exception& e)
  {
    LOG(DEBUG) << "Exception on send '" << e.what() << "', removing handler";
    remove(handler);
  }
}

}
