!INCLUDE ..\..\..\Makefile.VS.common

YAMI_LIBS = ..\..\..\..\lib\yamicpp.lib ..\..\..\..\lib\yamicore.lib
YAMI_INCLUDE = ..\..\..\..\include
PLATFORM_INCLUDE = ..\common_utils\windows

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(YAMI_INCLUDE) /I $(PLATFORM_INCLUDE) $(OPEN_SSL_OPTIONS)

EXAMPLES = \
	publisher.exe \
	subscriber.exe

all : $(EXAMPLES)

publisher_with_stats.exe : publisher_with_stats.cpp

.cpp.exe:
	$(CPP) $(CPP_OPTIONS) $< /link $(YAMI_LIBS) $(OPEN_SSL_LIBS) Ws2_32.lib
	
clean :
	del /Q $(EXAMPLES) *.obj
