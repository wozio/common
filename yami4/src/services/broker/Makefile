COMPILER = c++
OPTIONS = -O2
#OPTIONS = -Wall -Wextra -W -Wshadow -pedantic -Wno-long-long
INCLUDES = -I../../../include -I/usr/local/include
LIBDIRS = -L../../../lib -L/usr/local/lib
LIBS = -lyamicore -lboost_thread -lboost_system -lpthread

all : yami4broker

OBJ_FILES = main.o \
	broker-configuration.o \
	broker-ids.o \
	broker-messaging.o \
	broker-routing.o \
	patterns.o \
	../common/log.o \
	../common/properties.o \
	../common/utils.o

yami4broker : $(OBJ_FILES)
	$(COMPILER) -o $@ $(OBJ_FILES) $(LIBDIRS) $(LIBS)

.cpp.o:
	$(COMPILER) $(OPTIONS) $(INCLUDES) -c -o $@ $<


clean :
	rm -f yami4broker *.o
