#ifndef APP_H
#define	APP_H

#include <boost/property_tree/ptree.hpp>
#include <functional>
#include <vector>
#include <string>

namespace home_system
{

typedef std::function<void (const std::vector<std::string>&)> cmd_handler_type;

class app
{
public:
  app(const char* conf_file, bool daemonize, cmd_handler_type cmd_handler = nullptr);
  app(bool daemonize, cmd_handler_type cmd_handler = nullptr);
  virtual ~app();
  
  int run();
  
  static boost::property_tree::ptree& config();

private:
  static boost::property_tree::ptree config_;
  bool daemonize_;
  cmd_handler_type cmd_handler_;
};

}

#endif	/* APP_H */

