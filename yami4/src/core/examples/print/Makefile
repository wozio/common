INCLUDE_DIRS = -I../../../../include
PLATFORM_INCLUDE = -I../common_utils/posix
LIB_DIRS = -L../../../../lib
LIBS = -lyamicore -lpthread

build : client server

client : client.cpp
	c++ $? -o $@ $(INCLUDE_DIRS) $(PLATFORM_INCLUDE) $(LIB_DIRS) $(LIBS)

server : server.cpp
	c++ $? -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)


clean :
	rm -f client server
