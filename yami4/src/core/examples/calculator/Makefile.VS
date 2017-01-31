!INCLUDE ..\..\..\Makefile.VS.common

YAMI_LIB = ..\..\..\..\lib\yamicore.lib
YAMI_INCLUDE = ..\..\..\..\include

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(YAMI_INCLUDE) $(OPEN_SSL_OPTIONS)

EXAMPLES = \
	client.exe \
	server.exe

all : $(EXAMPLES)

.cpp.exe:
	$(CPP) $(CPP_OPTIONS) $< /link $(YAMI_LIB) $(OPEN_SSL_LIBS) Ws2_32.lib
	
clean :
	del /Q $(EXAMPLES) *.obj
