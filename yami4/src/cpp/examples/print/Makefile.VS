LIB_YAMI = ..\..\..\..\lib\yamicpp.lib ..\..\..\..\lib\yamicore.lib
INCLUDE_YAMI = ..\..\..\..\include

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(INCLUDE_YAMI)

EXAMPLES = \
	client.exe \
	client_synchronous.exe \
	server.exe

all : $(EXAMPLES)

.cpp.exe:
	$(CPP) $(CPP_OPTIONS) $< /link $(LIB_YAMI) Ws2_32.lib
	
clean :
	del /Q $(EXAMPLES) *.obj
