#include "handlers.h"
#include "logger.h"
#include "rapidjson/stringbuffer.h"
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
  LOG(DEBUG) << "Size of ws_to_handler_map=" << ws_to_handler_map_.size();
  LOG(DEBUG) << "Size of list=" << list_.size();
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

  LOG(DEBUG) << "Size of ws_to_handler_map=" << ws_to_handler_map_.size();
  LOG(DEBUG) << "Size of list=" << list_.size();
}

void handlers::select()
{
  if (list_.size() > 0)
  {
    Socket::SocketList read_list(list_);
    Socket::SocketList except_list(list_);
    Socket::SocketList write_list;
    
    for (auto s : ws_to_handler_map_)
    {
      if (s.second->something_to_send())
      {
        write_list.push_back(s.first);
      }
    }

    Socket::select(read_list, write_list, except_list, Timespan(0, 100000));
    
    //LOG(TRACE) << read_list.size() << " " << write_list.size() << " " << except_list.size();
    
    for (auto socket : read_list)
    {
      auto handler = ws_to_handler_map_[socket];
      // start reading from associated websocket
      ios_.io_service().post([this, handler] ()
        {
          this->read(handler);
        }
      );
    }
    for (auto socket : write_list)
    {
      auto handler = ws_to_handler_map_[socket];
      // start reading from associated websocket
      ios_.io_service().post([this, handler] ()
        {
          this->send(handler);
        }
      );
    }
    for (auto socket : except_list)
    {
      auto handler = ws_to_handler_map_[socket];
      // start reading from assiociated websocket
      ios_.io_service().post([this, handler] ()
        {
          this->remove(handler);
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
    handler::type_t t;
    int n = handler->read(data, t);
    if (n > 0)
    {
      handler->on_read(data, n, t);
    }
  }
  catch (const exception& e)
  {
    LOG(DEBUG) << "Exception on read '" << e.what() << "', removing handler";
    remove(handler);
  }
}

void handlers::send(handler_t handler)
{
  try
  {
    handler->send();
  }
  catch (const Poco::Exception& e)
  {
    LOG(DEBUG) << "Exception on send '" << e.displayText() << "', removing handler";
    remove(handler);
  }
  catch (const exception& e)
  {
    LOG(DEBUG) << "Exception on send '" << e.what() << "', removing handler";
    remove(handler);
  }
}

}
