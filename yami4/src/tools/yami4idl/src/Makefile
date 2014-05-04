COMPILER = c++
#OPTIONS = -Wall -Wextra -W -Wshadow -pedantic -Wno-long-long
INCLUDES = -I/usr/local/include
LIBDIRS = -L/usr/local/lib
LIBS = -lboost_filesystem -lboost_system

all : yami4idl

OBJ_FILES = main.o \
	idl.o \
	ada_generator.o \
	cpp_generator.o \
	idl_generator.o \
	java_generator.o \
	name_utils.o \
	parser.o \
	reader.o \
	structures.o \
	tokenizer.o

yami4idl : $(OBJ_FILES)
	$(COMPILER) -o $@ $(OBJ_FILES) $(LIBDIRS) $(LIBS)

.cpp.o:
	$(COMPILER) $(OPTIONS) $(INCLUDES) -c -o $@ $<


clean :
	rm -f yami4idl *.o
