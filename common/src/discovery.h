#ifndef DISCOVERY_H
#define	DISCOVERY_H

#include "ios_wrapper.h"
#include <functional>
#include <memory>
#include <map>
#include <set>

namespace home_system
{

class discovery;

typedef std::unique_ptr<home_system::discovery> discovery_t;

class service;

typedef std::function<void (const std::string&, bool) > subsription;
typedef std::map<size_t, subsription> subsriptions;

class discovery
{
public:

  typedef std::function<void(const std::string&)> log_callback_t;
  static discovery_t create(log_callback_t log_callback = nullptr)
  {
    return discovery_t(new discovery(log_callback));
  };

  discovery(log_callback_t log_callback);
  ~discovery();
  
  std::string get(const std::string& name);
  void get_all(std::map<std::string, std::string>& services);
  std::map<std::string, std::string> get_all();

  void subscribe(service* s);
  void unsubscribe(service* s);

  size_t subscribe(subsription callback);
  void unsubscribe(size_t subscription_id);

  void on_connection_closed(const char* endpoint);

private:
  log_callback_t log_callback_;

  // known external services (key is name, value is yamie endpoint)
  std::map<std::string, std::string> known_services_;

  // external services supervision
  // notify received from service within last idle period (key is id)
  std::map<std::string, bool> notify_received_;
  home_system::ios_wrapper ios_;
  boost::asio::deadline_timer idle_dt_;
  std::mutex idle_mutex;

  void on_idle_timeout(const boost::system::error_code& error);


  void check_service(const std::string& name, const std::string& ye);
  void store_service(const std::string& name, const std::string& ye);
  void erase_service(const std::string& name);

  // multicast receive
  boost::asio::ip::udp::endpoint listen_endpoint_;
  boost::asio::ip::udp::socket listen_socket_;

  enum
  {
    msg_max_size = 65507
  };
  char data_[msg_max_size];

  void handle_receive(const boost::system::error_code& error, size_t bytes_recvd);

  void handle_hello(const std::vector<std::string>& fields);
  void handle_notify(const std::vector<std::string>& fields);
  void handle_bye(const std::vector<std::string>& fields);
  void handle_search(const std::vector<std::string>& fields);

  void send_notify();

  // service avaliability subscriptions, key is name of the service
  std::set<service*> on_service_subscriptions;
  subsriptions subscriptions_;
};

class service_not_found
: public std::exception
{
public:
  service_not_found(const service_not_found& o)
  : name_(o.name_)
  {
  }

  service_not_found(const std::string& name)
  : name_(name)
  {
  }

  const char* what() const noexcept
  {
    return name_.c_str();
  }

private:
  std::string name_;
};

}

extern home_system::discovery_t _discovery;

#define DISCOVERY (*::_discovery)

#endif	/* DISCOVERY_H */

