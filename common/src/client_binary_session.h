#pragma once
#include <boost/signals2.hpp>
#include <yami4-cpp/yami.h>

namespace home_system
{
  class client_binary_session
  {
  public:
    enum class event_t {
      created,
      destroyed,
      stopped,
      started
    };
    client_binary_session();
    ~client_binary_session();

    boost::signals2::signal<void(const std::vector<char>& data)> data_;
    boost::signals2::signal<void(event_t event)> event_;

    std::string get_endpoint();

    void operator()(yami::incoming_message & im);

  private:
    std::string endpoint_;
    std::unique_ptr<yami::agent> agent_;
  };
}
