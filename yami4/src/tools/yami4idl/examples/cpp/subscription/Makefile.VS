!INCLUDE ..\..\..\..\..\Makefile.VS.common

YAMI_LIB = ..\..\..\..\..\..\lib\yamicpp.lib ..\..\..\..\..\..\lib\yamicore.lib
YAMI_INCLUDE = ..\..\..\..\..\..\include
PLATFORM_INCLUDE = ..\common_utils\windows

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(YAMI_INCLUDE) /I $(PLATFORM_INCLUDE)

EXAMPLES = \
	publisher.exe \
	subscriber.exe

all : ydl $(EXAMPLES)

ydl :
	..\..\..\src\yami4idl subscription.ydl -cpp

publisher.exe :
	$(CPP) $(CPP_OPTIONS) publisher.cpp subscription.cpp /link $(YAMI_LIB) $(OPEN_SSL_LIBS) Ws2_32.lib
	
subscriber.exe :
	$(CPP) $(CPP_OPTIONS) subscriber.cpp subscription.cpp /link $(YAMI_LIB) $(OPEN_SSL_LIBS) Ws2_32.lib

	
clean :
	del /Q $(EXAMPLES) subscription.h subscription.cpp *.obj
