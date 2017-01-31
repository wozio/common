COMPILER = c++

# These options are used for development
OPTIONS = -Wall -Wextra -W -Wshadow -pedantic -Wno-long-long -g

# These options are intended for final use
#OPTIONS = -O2 $(USER_COMPILER_OPTIONS)

PLATFORM = posix
INCLUDE_PLATFORM = -I$(PLATFORM) -I../core/$(PLATFORM)
CORE_INCLUDES = -I../../include

PUBLIC_HEADERS = activity_statistics_monitor.h \
	agent.h \
	agent_impl_base.h \
	connection_event.h \
	connection_event_dispatcher_base.h \
	connection_event_generic_dispatcher.h \
	errors.h \
	event_callback.h \
	incoming_message.h \
	incoming_message_dispatcher_base.h \
	incoming_message_generic_dispatcher.h \
	io_error_dispatcher_base.h \
	io_error_generic_dispatcher.h \
	message_state.h \
	option_names.h \
	outgoing_message.h \
	outgoing_message_dispatcher_base.h \
	outgoing_message_generic_dispatcher.h \
	parameter_entry.h \
	parameter_type.h \
	parameters.h \
	raw_buffer_data_source.h \
	serializable.h \
	value_publisher.h \
	value_publisher_overflow_action.h \
	value_publisher_overflow_dispatcher_base.h \
	value_publisher_overflow_generic_dispatcher.h \
	version.h \
	yami.h

OBJECT_FILES = activity_statistics_monitor.o \
	agent.o \
	agent_impl.o \
	details.o \
	id_generator.o \
	incoming_message.o \
	incoming_message_queue.o \
	name_resolver.o \
	options.o \
	outgoing_message.o \
	outgoing_message_info.o \
	outgoing_message_manager.o \
	parameter_entry.o \
	parameters.o \
	raw_buffer_data_source.o \
	value_publisher.o \
	value_publisher_impl.o \
	version.o \
	water_flow_manager.o

PLATFORM_OBJECT_FILES = $(PLATFORM)/delay.o \
	$(PLATFORM)/flag.o \
	$(PLATFORM)/semaphore.o \
	$(PLATFORM)/start_thread.o

OUTPUT = libyamicpp$(LIBNAME_POSTFIX).a

all : $(OUTPUT)
	cp $(OUTPUT) ../../lib
	cp $(PUBLIC_HEADERS) ../../include/yami4-cpp

$(OUTPUT) : $(OBJECT_FILES) $(PLATFORM_OBJECT_FILES)
	ar rsv $@ $(OBJECT_FILES) $(PLATFORM_OBJECT_FILES)


$(PLATFORM)/delay.o : $(PLATFORM)/delay.cpp
	$(COMPILER) $(OPTIONS) $(CORE_INCLUDES) $(INCLUDE_PLATFORM) -c -o $@ $?

$(PLATFORM)/flag.o : $(PLATFORM)/flag.cpp
	$(COMPILER) $(OPTIONS) $(CORE_INCLUDES) $(INCLUDE_PLATFORM) -c -o $@ $?

$(PLATFORM)/semaphore.o : $(PLATFORM)/semaphore.cpp
	$(COMPILER) $(OPTIONS) $(CORE_INCLUDES) $(INCLUDE_PLATFORM) -c -o $@ $?

$(PLATFORM)/start_thread.o : $(PLATFORM)/start_thread.cpp
	$(COMPILER) $(OPTIONS) $(CORE_INCLUDES) $(INCLUDE_PLATFORM) -c -o $@ $?


.cpp.o:
	$(COMPILER) $(OPTIONS) $(CORE_INCLUDES) $(INCLUDE_PLATFORM) -c -o $@ $<


clean : clean_objects
	rm -f $(OUTPUT)

clean_objects :
	rm -f *.o $(PLATFORM)/*.o
