!INCLUDE ..\..\..\Makefile.VS.common

BOOST_LIBS = $(BOOST_LIBDIR)\libboost_filesystem$(BOOST_LIB_SUFFIX).lib \
	$(BOOST_LIBDIR)\libboost_system$(BOOST_LIB_SUFFIX).lib

CPP = cl.exe
CPP_OPTIONS = /c /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(BOOST_INCLUDE)

OBJ_FILES = main.obj \
	idl.obj \
	ada_generator.obj \
	cpp_generator.obj \
	idl_generator.obj \
	java_generator.obj \
	name_utils.obj \
	parser.obj \
	reader.obj \
	structures.obj \
	tokenizer.obj

EXEC = yami4idl.exe
    
all : $(EXEC)

$(EXEC) : $(OBJ_FILES)
	$(CPP) /Fe: $(EXEC) $(OBJ_FILES) /link $(BOOST_LIBS)

.cpp.obj::
	$(CPP) $(CPP_OPTIONS) $<

clean :
	del /Q $(EXEC) $(OBJ_FILES)
