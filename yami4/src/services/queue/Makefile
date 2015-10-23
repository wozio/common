COMPILER = c++
OPTIONS = -O2
#OPTIONS = -Wall -Wextra -W -Wshadow -pedantic -Wno-long-long
INCLUDES = -I../../../include -I/usr/local/include
LIBDIRS = -L../../../lib -L/usr/local/lib
LIBS = -lyamicore -lboost_thread -lboost_system -lpthread

all : yami4queue

OBJ_FILES = main.o \
	queue-configuration.o \
	queue-messaging.o \
	queue-routing.o \
	../common/log.o \
	../common/properties.o \
	../common/utils.o

yami4queue : $(OBJ_FILES)
	$(COMPILER) -o $@ $(OBJ_FILES) $(LIBDIRS) $(LIBS)

.cpp.o:
	$(COMPILER) $(OPTIONS) $(INCLUDES) -c -o $@ $<


clean :
	rm -f yami4queue *.o
