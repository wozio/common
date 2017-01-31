INCLUDE_DIRS = -I../../../../include -I../common_utils/posix
LIB_DIRS = -L../../../../lib
LIBS = -lyamicpp -lyamicore -lpthread

build : publisher subscriber

publisher : publisher.cpp
	c++ $? -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

subscriber : subscriber.cpp
	c++ $? -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

publisher_with_stats : publisher_with_stats.cpp
	c++ $? -g -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)


clean :
	rm -f publisher subscriber
