#include "app.h"

#ifdef __linux__
#include <signal.h>
#include <sys/stat.h>
#endif
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <string>

using namespace std;

namespace home_system
{
  namespace app
  {
    void prepare(bool daemonize)
    {
#ifdef __linux__
      if (daemonize)
      {
        cout << "Running as daemon" << endl;
        
        pid_t pid = fork();
        if (pid < 0)
        {
          LOG(ERROR) << "Cannot fork";
          exit(EXIT_FAILURE);
        }
        else if (pid > 0)
        {
          exit(EXIT_SUCCESS);
        }

        umask(0);

        pid_t sid = setsid();
        if (sid < 0)
        {
          LOG(ERROR) << "Cannot setsid";
          exit(EXIT_FAILURE);
        }
        
        fclose(stdin);
        fclose(stdout);
        fclose(stderr);
      }
  
#endif
    }

    int run(bool daemonize)
    {
#ifdef __linux__
      if (daemonize)
      {
        sigset_t sset;
        sigemptyset(&sset);
        sigaddset(&sset, SIGQUIT);
        sigaddset(&sset, SIGTERM);
        sigprocmask(SIG_BLOCK, &sset, NULL);
        int sig;
        sigwait(&sset, &sig);
      }
      else
#endif
      {
        cout << "Enter q to quit..." << endl;
        std::string input_line;
        while (std::getline(std::cin, input_line))
        {
          if (input_line == "q" || input_line == "quit")
          {
            break;
          }
        }
      }
      return 0;
    }
  }
}
