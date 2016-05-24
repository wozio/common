!INCLUDE ..\..\Makefile.VS.common

CPP = cl.exe
CPP_OPTIONS = /c /nologo /EHsc /MDd /D _CRT_SECURE_NO_WARNINGS /I . $(OPEN_SSL_OPTIONS)

ARCHIVE_OBJS = \
	channel.obj \
	io_error_handler.obj \
	listener.obj \
	mutex.obj \
	network_utils.obj \
	selector.obj

all : $(ARCHIVE_OBJS)

.cpp.obj::
	$(CPP) $(CPP_OPTIONS) $< 

clean :
	del /Q *.obj
