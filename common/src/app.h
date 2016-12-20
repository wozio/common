#ifndef APP_H
#define	APP_H

#include <boost/property_tree/ptree.hpp>

namespace home_system
{

class app
{
public:
  app(const char* conf_file, bool daemonize);
  app(bool daemonize);
  virtual ~app();
  
  int run();
  
  static boost::property_tree::ptree& config();

private:
  static boost::property_tree::ptree config_;
  bool daemonize_;
};

}

#endif	/* APP_H */

