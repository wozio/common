!INCLUDE ..\Makefile.VS.common

OUTDIR = ..\..\lib
PUBLIC_HEADERS_DIR = ..\..\include\yami4-cpp

YAMI_LIB = $(OUTDIR)\yamicpp.lib

CPP = cl.exe
CPP_OPTIONS = /c /nologo /EHsc /MDd /D _CRT_SECURE_NO_WARNINGS \
	/I ..\..\include /I windows /I ..\core\windows $(OPEN_SSL_OPTIONS)

ARCHIVE = lib.exe
ARCHIVE_FLAGS = /nologo /out:$(YAMI_LIB)

ARCHIVE_OBJS = \
	activity_statistics_monitor.obj \
	agent.obj \
	agent_impl.obj \
	details.obj \
	id_generator.obj \
	incoming_message.obj \
	incoming_message_queue.obj \
	name_resolver.obj \
	options.obj \
	outgoing_message.obj \
	outgoing_message_info.obj \
	outgoing_message_manager.obj \
	parameter_entry.obj \
	parameters.obj \
	raw_buffer_data_source.obj \
	value_publisher.obj \
	value_publisher_impl.obj \
	version.obj \
	water_flow_manager.obj \
	windows\delay.obj \
	windows\flag.obj \
	windows\semaphore.obj \
	windows\start_thread.obj

all : platform_objects $(YAMI_LIB)

platform_objects :
	cd windows
	nmake /F Makefile.VS
	cd ..

$(YAMI_LIB) : $(ARCHIVE_OBJS)
	$(ARCHIVE) $(ARCHIVE_FLAGS) $(ARCHIVE_OBJS)
	copy activity_statistics_monitor.h $(PUBLIC_HEADERS_DIR)
	copy agent.h $(PUBLIC_HEADERS_DIR)
	copy agent_impl_base.h $(PUBLIC_HEADERS_DIR)
	copy connection_event.h $(PUBLIC_HEADERS_DIR)
	copy connection_event_dispatcher_base.h $(PUBLIC_HEADERS_DIR)
	copy connection_event_generic_dispatcher.h $(PUBLIC_HEADERS_DIR)
	copy errors.h $(PUBLIC_HEADERS_DIR)
	copy event_callback.h $(PUBLIC_HEADERS_DIR)
	copy incoming_message.h $(PUBLIC_HEADERS_DIR)
	copy incoming_message_dispatcher_base.h $(PUBLIC_HEADERS_DIR)
	copy incoming_message_generic_dispatcher.h $(PUBLIC_HEADERS_DIR)
	copy io_error_dispatcher_base.h $(PUBLIC_HEADERS_DIR)
	copy io_error_generic_dispatcher.h $(PUBLIC_HEADERS_DIR)
	copy message_state.h $(PUBLIC_HEADERS_DIR)
	copy option_names.h $(PUBLIC_HEADERS_DIR)
	copy outgoing_message.h $(PUBLIC_HEADERS_DIR)
	copy outgoing_message_dispatcher_base.h $(PUBLIC_HEADERS_DIR)
	copy outgoing_message_generic_dispatcher.h $(PUBLIC_HEADERS_DIR)
	copy parameter_entry.h $(PUBLIC_HEADERS_DIR)
	copy parameter_type.h $(PUBLIC_HEADERS_DIR)
	copy parameters.h $(PUBLIC_HEADERS_DIR)
	copy raw_buffer_data_source.h $(PUBLIC_HEADERS_DIR)
	copy serializable.h $(PUBLIC_HEADERS_DIR)
	copy value_publisher.h $(PUBLIC_HEADERS_DIR)
	copy value_publisher_overflow_action.h $(PUBLIC_HEADERS_DIR)
	copy value_publisher_overflow_dispatcher_base.h $(PUBLIC_HEADERS_DIR)
	copy value_publisher_overflow_generic_dispatcher.h $(PUBLIC_HEADERS_DIR)
	copy version.h $(PUBLIC_HEADERS_DIR)
	copy yami.h $(PUBLIC_HEADERS_DIR)

.cpp.obj::
	$(CPP) $(CPP_OPTIONS) $< 

clean :
	cd windows
	nmake /F Makefile.VS clean
	cd ..
	del /Q *.obj
	del /Q $(YAMI_LIB)
