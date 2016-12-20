COMPILER = c++
#OPTIONS = -O2
OPTIONS = -Wall -Wextra -W -Wshadow -pedantic -Wno-long-long
INCLUDES = -I../../../../include -I/usr/local/include
LIBDIRS = -L../../../../lib -L/usr/local/lib
LIBS = -lyamicpp -lpthread -lyamicore 

all : publisher_text subscriber_text get_stats

publisher_text : publisher_text.cpp
	$(COMPILER) -o $@ publisher_text.cpp ../../common/utils.cpp \
		$(OPTIONS) $(INCLUDES) $(LIBDIRS) $(LIBS)

subscriber_text : subscriber_text.cpp
	$(COMPILER) -o $@ $? $(OPTIONS) $(INCLUDES) $(LIBDIRS) $(LIBS)

get_stats : get_stats.cpp
	$(COMPILER) -o $@ $? $(OPTIONS) $(INCLUDES) $(LIBDIRS) $(LIBS)


clean :
	rm -f publisher_text subscriber_text get_stats
