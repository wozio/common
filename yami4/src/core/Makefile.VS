!INCLUDE ..\Makefile.VS.common

OUTDIR = ..\..\lib
PUBLIC_HEADERS_DIR = ..\..\include\yami4-core

YAMI_LIB = $(OUTDIR)\yamicore.lib

CPP = cl.exe
CPP_OPTIONS = /c /nologo /EHsc /MDd /D _CRT_SECURE_NO_WARNINGS /I windows $(OPEN_SSL_OPTIONS)

ARCHIVE = lib.exe
ARCHIVE_FLAGS = /nologo /out:$(YAMI_LIB)

ARCHIVE_OBJS = \
	agent.obj \
	allocator.obj \
	c_interface.obj \
	channel.obj \
	channel_group.obj \
	channel_holder.obj \
	fatal_errors.obj \
	listener.obj \
	network_utils.obj \
	options.obj \
	parameter_entry.obj \
	parameter_iterator.obj \
	parameters-details.obj \
	parameters.obj \
	raw_buffer_data_source.obj \
	serialization.obj \
	windows\channel.obj \
	windows\io_error_handler.obj \
	windows\listener.obj \
	windows\mutex.obj \
	windows\network_utils.obj \
	windows\selector.obj

all : platform_objects $(YAMI_LIB)

platform_objects :
	cd windows
	nmake /F Makefile.VS
	cd ..

$(YAMI_LIB) : $(ARCHIVE_OBJS)
	$(ARCHIVE) $(ARCHIVE_FLAGS) $(ARCHIVE_OBJS)
	copy agent.h $(PUBLIC_HEADERS_DIR)
	copy allocator.h $(PUBLIC_HEADERS_DIR)
	copy channel_descriptor.h $(PUBLIC_HEADERS_DIR)
	copy core.h $(PUBLIC_HEADERS_DIR)
	copy dll.h $(PUBLIC_HEADERS_DIR)
	copy fatal_errors.h $(PUBLIC_HEADERS_DIR)
	copy option_names.h $(PUBLIC_HEADERS_DIR)
	copy parameter_entry.h $(PUBLIC_HEADERS_DIR)
	copy parameter_iterator.h $(PUBLIC_HEADERS_DIR)
	copy parameter_type.h $(PUBLIC_HEADERS_DIR)
	copy parameters.h $(PUBLIC_HEADERS_DIR)
	copy raw_buffer_data_source.h $(PUBLIC_HEADERS_DIR)
	copy serializable.h $(PUBLIC_HEADERS_DIR)

.cpp.obj::
	$(CPP) $(CPP_OPTIONS) $< 

clean :
	cd windows
	nmake /F Makefile.VS clean
	cd ..
	del /Q *.obj
	del /Q $(YAMI_LIB)
