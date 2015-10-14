COMPILER = c++
OPTIONS = -O2
#OPTIONS = -Wall -Wextra -W -Wshadow -pedantic -Wno-long-long
INCLUDES = -I../../../include -I/usr/local/include
LIBDIRS = -L../../../lib -L/usr/local/lib
LIBS = -lyamicore -lboost_filesystem -lboost_system -lboost_thread -lpthread

all : yami4names

OBJ_FILES = main.o \
	name_server-configuration.o \
	name_server-messaging.o \
	name_server-storage.o \
	../common/log.o \
	../common/properties.o \
	../common/utils.o

yami4names : $(OBJ_FILES)
	$(COMPILER) -o $@ $(OBJ_FILES) $(LIBDIRS) $(LIBS)

.cpp.o:
	$(COMPILER) $(OPTIONS) $(INCLUDES) -c -o $@ $<


clean :
	rm -f yami4names *.o
