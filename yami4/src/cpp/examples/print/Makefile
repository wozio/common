INCLUDE_DIRS = -I../../../../include
LIB_DIRS = -L../../../../lib
LIBS = -lyamicpp -lyamicore -lpthread

build : client client_synchronous server

client : client.cpp
	c++ $? -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

client_synchronous : client_synchronous.cpp
	c++ $? -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

server : server.cpp
	c++ $? -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)


clean :
	rm -f client client_synchronous server
