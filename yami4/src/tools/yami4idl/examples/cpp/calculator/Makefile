INCLUDE_DIRS = -I../../../../../../include
LIB_DIRS = -L../../../../../../lib
LIBS = -lyamicpp -lyamicore -lpthread

build : ydl client server

ydl :
	../../../src/yami4idl calculator.ydl -cpp

client : client.cpp calculator.cpp
	c++ client.cpp calculator.cpp -o $@ $(OPTIONS) $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

server : server.cpp calculator.cpp
	c++ server.cpp calculator.cpp -o $@ $(OPTIONS) $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)


clean :
	rm -f client server calculator.h calculator.cpp

