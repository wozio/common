COMPILER = c++
#OPTIONS = -O2
OPTIONS = -Wall -Wextra -W -Wshadow -pedantic -Wno-long-long
INCLUDES = -I../../../../include -I/usr/local/include
LIBDIRS = -L../../../../lib -L/usr/local/lib
LIBS = -lyamicpp -lpthread -lyamicore 

all : bind resolve list

bind : bind.cpp
	$(COMPILER) -o $@ $? $(OPTIONS) $(INCLUDES) $(LIBDIRS) $(LIBS)

resolve : resolve.cpp
	$(COMPILER) -o $@ $? $(OPTIONS) $(INCLUDES) $(LIBDIRS) $(LIBS)

list : list.cpp
	$(COMPILER) -o $@ $? $(OPTIONS) $(INCLUDES) $(LIBDIRS) $(LIBS)


clean :
	rm -f bind resolve list
