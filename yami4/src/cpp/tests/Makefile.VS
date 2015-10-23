!INCLUDE ..\..\Makefile.VS.common

YAMI_LIBS = ..\..\..\lib\yamicore.lib ..\..\..\lib\yamicpp.lib
YAMI_INCLUDE = ..\..\..\include

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(YAMI_INCLUDE)

TESTS = \
	parameters-test.exe \
	agent-test.exe

all : $(TESTS) run

run :
	parameters-test
	agent-test

notifications-test.exe : notifications-test.cpp

.cpp.exe:
	$(CPP) $(CPP_OPTIONS) $< /link $(YAMI_LIBS) $(OPEN_SSL_LIBS) Ws2_32.lib
	
clean :
	del /Q $(TESTS) *.obj
