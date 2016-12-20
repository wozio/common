!INCLUDE ..\..\..\..\..\Makefile.VS.common

YAMI_LIB = ..\..\..\..\..\..\lib\yamicpp.lib ..\..\..\..\..\..\lib\yamicore.lib
YAMI_INCLUDE = ..\..\..\..\..\..\include

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(YAMI_INCLUDE)

EXAMPLES = \
	client.exe \
	client_synchronous.exe \
	server.exe

all : ydl $(EXAMPLES)

ydl :
	..\..\..\src\yami4idl print.ydl -cpp

client.exe :
	$(CPP) $(CPP_OPTIONS) client.cpp print.cpp /link $(YAMI_LIB) $(OPEN_SSL_LIBS) Ws2_32.lib
	
client_synchronous.exe :
	$(CPP) $(CPP_OPTIONS) client_synchronous.cpp print.cpp /link $(YAMI_LIB) $(OPEN_SSL_LIBS) Ws2_32.lib
	
server.exe :
	$(CPP) $(CPP_OPTIONS) server.cpp print.cpp /link $(YAMI_LIB) $(OPEN_SSL_LIBS) Ws2_32.lib
	
	
clean :
	del /Q $(EXAMPLES) print.h print.cpp *.obj
