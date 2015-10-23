COMPILER = c++
#OPTIONS = -O2
OPTIONS = -Wall -Wextra -W -Wshadow -pedantic -Wno-long-long
INCLUDES = -I../../../../include -I/usr/local/include
LIBDIRS = -L../../../../lib -L/usr/local/lib
LIBS = -lyamicpp -lpthread -lyamicore 

all : get try_get put get_stats

get : get.cpp
	$(COMPILER) -o $@ $? $(OPTIONS) $(INCLUDES) $(LIBDIRS) $(LIBS)

try_get : try_get.cpp
	$(COMPILER) -o $@ $? $(OPTIONS) $(INCLUDES) $(LIBDIRS) $(LIBS)

put : put.cpp
	$(COMPILER) -o $@ $? $(OPTIONS) $(INCLUDES) $(LIBDIRS) $(LIBS)

get_stats : get_stats.cpp
	$(COMPILER) -o $@ $? $(OPTIONS) $(INCLUDES) $(LIBDIRS) $(LIBS)


clean :
	rm -f get try_get put get_stats
