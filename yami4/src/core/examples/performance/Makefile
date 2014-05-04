OPTIONS = -O2
INCLUDE_DIRS = -I../../../../include -I../common_utils/posix
LIB_DIRS = -L../../../../lib
LIBS = -lyamicore

build : client server

client : client.cpp
	c++ $? -o $@ $(OPTIONS) $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

server : server.cpp
	c++ $? -o $@ $(OPTIONS) $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)


clean :
	rm -f client server
