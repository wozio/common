OPTIONS = -O2
INCLUDE_DIRS = -I../../../../include
LIB_DIRS = -L../../../../lib
LIBS = -lyamicpp -lyamicore -lpthread

build : client server client_ns server_ns

client : client.cpp
	c++ $? -o $@ $(OPTIONS) $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

server : server.cpp
	c++ $? -o $@ $(OPTIONS) $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

client_ns : client_ns.cpp
	c++ $? -o $@ $(OPTIONS) $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

server_ns : server_ns.cpp
	c++ $? -o $@ $(OPTIONS) $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)


clean :
	rm -f client server client_ns server_ns
