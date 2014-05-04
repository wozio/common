LIB_YAMI = ..\..\..\..\..\..\lib\yamicpp.lib ..\..\..\..\..\..\lib\yamicore.lib
INCLUDE_YAMI = ..\..\..\..\..\..\include

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(INCLUDE_YAMI)

EXAMPLES = \
	client.exe \
	client_synchronous.exe \
	server.exe

all : ydl $(EXAMPLES)

ydl :
	..\..\..\src\yami4idl print.ydl -cpp

client.exe :
	$(CPP) $(CPP_OPTIONS) client.cpp print.cpp /link $(LIB_YAMI) Ws2_32.lib
	
client_synchronous.exe :
	$(CPP) $(CPP_OPTIONS) client_synchronous.cpp print.cpp /link $(LIB_YAMI) Ws2_32.lib
	
server.exe :
	$(CPP) $(CPP_OPTIONS) server.cpp print.cpp /link $(LIB_YAMI) Ws2_32.lib
	
	
clean :
	del /Q $(EXAMPLES) *.obj
