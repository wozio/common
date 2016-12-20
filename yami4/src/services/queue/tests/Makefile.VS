!INCLUDE ..\..\..\Makefile.VS.common

YAMI_LIBS = ..\..\..\..\lib\yamicpp.lib ..\..\..\..\lib\yamicore.lib
YAMI_INCLUDE = ..\..\..\..\include

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(YAMI_INCLUDE)

EXAMPLES = \
	get.exe \
	get_stats.exe \
	put.exe \
	try_get.exe

all : $(EXAMPLES)

.cpp.exe:
	$(CPP) $(CPP_OPTIONS) $< /link $(YAMI_LIBS) $(OPEN_SSL_LIBS) Ws2_32.lib
	
clean :
	del /Q $(EXAMPLES) *.obj
