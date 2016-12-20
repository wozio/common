INCLUDE_DIRS = -I../../include
LIB_DIRS = -L../../lib
LIBS = -lyamicpp-fPIC -lyamicore-fPIC -lpthread

UNAME = $(shell uname)

ifeq ($(UNAME),Darwin)
	SHARED_LINK_FLAGS = -dynamiclib -flat_namespace -undefined suppress
	SHARED_LIB_NAME = libyami4py.dylib
else
	SHARED_LINK_FLAGS = -shared
	SHARED_LIB_NAME = libyami4py.so
endif


$(SHARED_LIB_NAME) : corelib cpplib yami4py.o
	c++ $(SHARED_LINK_FLAGS) -o $(SHARED_LIB_NAME) yami4py.o \
	$(LIB_DIRS) $(LIBS)
	cp $(SHARED_LIB_NAME) ../../lib
	cp yami.py ../../lib

yami4py.o : yami4py.cpp
	c++ -c -fPIC -o $@ $? $(INCLUDE_DIRS)

corelib :
	cd ../core; \
	make clean_objects; \
	make LIBNAME_POSTFIX=-fPIC USER_COMPILER_OPTIONS=-fPIC; \
	make clean_objects

cpplib :
	cd ../cpp; \
	make clean_objects; \
	make LIBNAME_POSTFIX=-fPIC USER_COMPILER_OPTIONS=-fPIC; \
	make clean_objects

clean :
	rm -f libyami4py.* yami4py.o
