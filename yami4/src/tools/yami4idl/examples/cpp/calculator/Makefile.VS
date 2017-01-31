!INCLUDE ..\..\..\..\..\Makefile.VS.common

YAMI_LIB = ..\..\..\..\..\..\lib\yamicpp.lib ..\..\..\..\..\..\lib\yamicore.lib
YAMI_INCLUDE = ..\..\..\..\..\..\include

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(YAMI_INCLUDE)

EXAMPLES = \
	client.exe \
	server.exe

all : ydl $(EXAMPLES)

ydl :
	..\..\..\src\yami4idl calculator.ydl -cpp

client.exe :
	$(CPP) $(CPP_OPTIONS) client.cpp calculator.cpp /link $(YAMI_LIB) $(OPEN_SSL_LIBS) Ws2_32.lib
	
server.exe :
	$(CPP) $(CPP_OPTIONS) server.cpp calculator.cpp /link $(YAMI_LIB) $(OPEN_SSL_LIBS) Ws2_32.lib
	
	
clean :
	del /Q $(EXAMPLES) calculator.h calculator.cpp *.obj
