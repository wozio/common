INCLUDE_DIRS = -I../../../../../../include -I../common_utils/posix
LIB_DIRS = -L../../../../../../lib
LIBS = -lyamicpp -lyamicore -lpthread

build : ydl publisher subscriber

ydl :
	../../../src/yami4idl subscription.ydl -cpp

publisher : publisher.cpp
	c++ $? subscription.cpp -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

subscriber : subscriber.cpp
	c++ $? subscription.cpp -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)


clean :
	rm -f publisher subscriber subscription.h subscription.cpp

