#pragma once

#include <yami4-cpp\yami.h>

namespace home_system
{
  class server_binary_session
  {
  public:
    server_binary_session(const std::string& target_endpoint);
    ~server_binary_session();

    void send(char* buf, size_t buf_size);
  private:
    std::string target_endpoint_;
    std::unique_ptr<yami::agent> agent_;
  };
}
