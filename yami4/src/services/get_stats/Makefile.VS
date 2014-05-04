LIB_YAMI = ..\..\..\lib\yamicpp.lib ..\..\..\lib\yamicore.lib
INCLUDE_YAMI = ..\..\..\include

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /O2 /D _CRT_SECURE_NO_WARNINGS /I $(INCLUDE_YAMI)

EXAMPLES = \
	get_stats.exe

all : $(EXAMPLES)

.cpp.exe:
	$(CPP) $(CPP_OPTIONS) $< /link $(LIB_YAMI) Ws2_32.lib
	
clean :
	del /Q $(EXAMPLES) *.obj
