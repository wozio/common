INCLUDE_DIRS = -I../../../../../../include
LIB_DIRS = -L../../../../../../lib
LIBS = -lyamicpp -lyamicore -lpthread

build : ydl client client_synchronous server

ydl :
	../../../src/yami4idl print.ydl -cpp

client : client.cpp
	c++ $? print.cpp -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

client_synchronous : client_synchronous.cpp
	c++ $? print.cpp -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

server : server.cpp
	c++ $? print.cpp -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)


clean :
	rm -f client client_synchronous server print.h print.cpp

