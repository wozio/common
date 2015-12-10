COMPILER = c++

# These options are used for development
OPTIONS = -Wall -Wextra -W -Wshadow -Weffc++ -pedantic -Wno-long-long -g

# These options are used for development with OpenSSL
#OPTIONS = -DYAMI4_WITH_OPEN_SSL -Wall -Wextra -W -Wshadow -Weffc++ -pedantic -Wno-long-long -g

# These options are intended for final use
#OPTIONS = -O2 $(USER_COMPILER_OPTIONS)

# These options are intended for final use with OpenSSL
#OPTIONS = -O2 -DYAMI4_WITH_OPEN_SSL $(USER_COMPILER_OPTIONS)

PLATFORM = posix
INCLUDE_PLATFORM = -I$(PLATFORM)

PUBLIC_HEADERS = agent.h \
	allocator.h \
	channel_descriptor.h \
	core.h \
	dll.h \
	fatal_errors.h \
	option_names.h \
	parameter_entry.h \
	parameter_iterator.h \
	parameter_type.h \
	parameters.h \
	raw_buffer_data_source.h \
	serializable.h

OBJECT_FILES = agent.o \
	allocator.o \
	c_interface.o \
	channel.o \
	channel_group.o \
	channel_holder.o \
	fatal_errors.o \
	listener.o \
	network_utils.o \
	options.o \
	parameter_entry.o \
	parameter_iterator.o \
	parameters-details.o \
	parameters.o \
	raw_buffer_data_source.o \
	serialization.o

PLATFORM_OBJECT_FILES = $(PLATFORM)/channel.o \
	$(PLATFORM)/io_error_handler.o \
	$(PLATFORM)/listener.o \
	$(PLATFORM)/mutex.o \
	$(PLATFORM)/network_utils.o \
	$(PLATFORM)/selector.o

OUTPUT = libyamicore$(LIBNAME_POSTFIX).a


all : $(OUTPUT)
	cp $(OUTPUT) ../../lib
	cp $(PUBLIC_HEADERS) ../../include/yami4-core

$(OUTPUT) : $(OBJECT_FILES) $(PLATFORM_OBJECT_FILES)
	ar rsv $@ $(OBJECT_FILES) $(PLATFORM_OBJECT_FILES)

$(PLATFORM)/channel.o : $(PLATFORM)/channel.cpp
	$(COMPILER) $(OPTIONS) $(INCLUDE_PLATFORM) -c -o $@ $?

$(PLATFORM)/io_error_handler.o : $(PLATFORM)/io_error_handler.cpp
	$(COMPILER) $(OPTIONS) $(INCLUDE_PLATFORM) -c -o $@ $?

$(PLATFORM)/listener.o : $(PLATFORM)/listener.cpp
	$(COMPILER) $(OPTIONS) $(INCLUDE_PLATFORM) -c -o $@ $?

$(PLATFORM)/mutex.o : $(PLATFORM)/mutex.cpp
	$(COMPILER) $(OPTIONS) $(INCLUDE_PLATFORM) -c -o $@ $?

$(PLATFORM)/network_utils.o : $(PLATFORM)/network_utils.cpp
	$(COMPILER) $(OPTIONS) $(INCLUDE_PLATFORM) -c -o $@ $?

$(PLATFORM)/selector.o : $(PLATFORM)/selector.cpp
	$(COMPILER) $(OPTIONS) $(INCLUDE_PLATFORM) -c -o $@ $?


.cpp.o:
	$(COMPILER) $(OPTIONS) $(INCLUDE_PLATFORM) -c -o $@ $<


clean : clean_objects
	rm -f $(OUTPUT)

clean_objects :
	rm -f *.o $(PLATFORM)/*.o
