LIB_YAMI = ..\..\..\..\lib\yamicpp.lib ..\..\..\..\lib\yamicore.lib
INCLUDE_YAMI = ..\..\..\..\include

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS \
	/I $(INCLUDE_YAMI) /I ..\common_utils\windows

EXAMPLES = \
	publisher.exe \
	subscriber.exe

all : $(EXAMPLES)

publisher_with_stats.exe : publisher_with_stats.cpp

.cpp.exe:
	$(CPP) $(CPP_OPTIONS) $< /link $(LIB_YAMI) Ws2_32.lib
	
clean :
	del /Q $(EXAMPLES) *.obj
