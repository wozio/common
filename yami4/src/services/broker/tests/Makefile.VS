!INCLUDE ..\..\..\Makefile.VS.common

YAMI_LIBS = ..\..\..\..\lib\yamicpp.lib ..\..\..\..\lib\yamicore.lib
YAMI_INCLUDE = ..\..\..\..\include

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(YAMI_INCLUDE)

EXAMPLES = \
	get_stats.exe \
	publisher_text.exe \
	subscriber_text.exe

all : $(EXAMPLES)

publisher_text.exe : publisher_text.cpp ..\..\common\utils.cpp
	$(CPP) /Fe: publisher_text.exe $(CPP_OPTIONS) publisher_text.cpp ..\..\common\utils.cpp /link $(YAMI_LIBS) $(OPEN_SSL_LIBS) Ws2_32.lib

.cpp.exe:
	$(CPP) $(CPP_OPTIONS) $< /link $(YAMI_LIBS) $(OPEN_SSL_LIBS) Ws2_32.lib
	
clean :
	del /Q $(EXAMPLES) *.obj
