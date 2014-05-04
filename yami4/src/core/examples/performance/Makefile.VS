LIB_YAMI = ..\..\..\..\lib\yamicore.lib
INCLUDE_YAMI = ..\..\..\..\include

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS \
	/I $(INCLUDE_YAMI) /I ..\common_utils\windows

EXAMPLES = \
	client.exe \
	server.exe

all : $(EXAMPLES)

.cpp.exe:
	$(CPP) $(CPP_OPTIONS) $< /link $(LIB_YAMI) Ws2_32.lib
	
clean :
	del /Q $(EXAMPLES) *.obj
