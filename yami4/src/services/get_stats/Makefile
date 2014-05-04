OPTIONS = -O2
INCLUDE_DIRS = -I../../../include
LIB_DIRS = -L../../../lib
LIBS = -lyamicpp -lyamicore -lpthread

build : get_stats

get_stats : get_stats.cpp
	c++ $? -o $@ $(OPTIONS) $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)


clean :
	rm -f get_stats
