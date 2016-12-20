INCLUDE_DIRS = -I../../../../include
LIB_DIRS = -L../../../../lib
LIBS = -lyamicore

# This is for OpenSSL support:
LIBS += -lssl

build : client server

client : client.cpp
	c++ $? -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

server : server.cpp
	c++ $? -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

server_ssl : server_ssl.cpp
	c++ $? -g -DYAMI4_WITH_OPEN_SSL -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)


clean :
	rm -f client server server_ssl
