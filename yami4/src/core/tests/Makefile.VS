!INCLUDE ..\..\Makefile.VS.common

YAMI_LIB = ..\..\..\lib\yamicore.lib

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I ../windows $(OPEN_SSL_OPTIONS)

TESTS = \
	allocator-test.exe \
	parameters-test.exe \
	connect-test1.exe \
	message-test1.exe \
	message-test1-hard.exe \
	message-test2.exe \
	message-test3.exe \
	message-test4.exe

all : $(TESTS) run

run :
	allocator-test
	parameters-test
	connect-test1
	message-test1
	message-test1-hard
	message-test2
	message-test3
	message-test4

.cpp.exe:
	$(CPP) $(CPP_OPTIONS) $< /link $(YAMI_LIB) $(OPEN_SSL_LIBS) Ws2_32.lib
	
clean :
	del /Q $(TESTS) *.obj
