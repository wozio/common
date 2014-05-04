INCLUDE_DIRS = -I../../../../include
LIB_DIRS = -L../../../../lib
LIBS = -lyamicore

build : client server

client : client.cpp
	c++ $? -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

server : server.cpp
	c++ $? -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)


clean :
	rm -f client server
