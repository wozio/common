#include "server_binary_session.h"

namespace home_system
{
  server_binary_session::server_binary_session(std::string& target_endpoint)
  : target_endpoint_(target_endpoint)
  {
    agent_.reset(new yami::agent());
  }

  server_binary_session::~server_binary_session()
  {
    agent_.reset();
  }

  void server_binary_session::send(char* buf, size_t buf_size)
  {
    yami::raw_buffer_data_source raw_binary(buf, buf_size);
    agent_->send_one_way(target_endpoint_, "", "", raw_binary);
  }
}