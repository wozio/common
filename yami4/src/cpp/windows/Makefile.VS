CPP = cl.exe
INCLUDES = /I . /I ..\..\..\include
CPP_OPTIONS = /c /nologo /EHsc /MDd /D _CRT_SECURE_NO_WARNINGS $(OPEN_SSL_OPTIONS)

ARCHIVE_OBJS = \
	delay.obj \
	flag.obj \
	semaphore.obj \
	start_thread.obj

all : $(ARCHIVE_OBJS)

.cpp.obj::
	$(CPP) $(CPP_OPTIONS) $(INCLUDES) $< 

clean :
	del /Q *.obj
