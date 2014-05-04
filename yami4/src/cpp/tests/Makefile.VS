LIB_YAMI = ..\..\..\lib\yamicore.lib ..\..\..\lib\yamicpp.lib
INCLUDE_YAMI = ..\..\..\include

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(INCLUDE_YAMI)

TESTS = \
	parameters-test.exe \
	agent-test.exe

all : $(TESTS) run

run :
	parameters-test
	agent-test

notifications-test.exe : notifications-test.cpp

.cpp.exe:
	$(CPP) $(CPP_OPTIONS) $< /link $(LIB_YAMI) Ws2_32.lib
	
clean :
	del /Q $(TESTS) *.obj
