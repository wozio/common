!INCLUDE ..\..\Makefile.VS.common

YAMI_LIBS = ..\..\..\lib\yamicore.lib
YAMI_INCLUDE = ..\..\..\include

BOOST_LIBS = $(BOOST_LIBDIR)\libboost_thread$(BOOST_LIB_SUFFIX).lib \
	$(BOOST_LIBDIR)\libboost_date_time$(BOOST_LIB_SUFFIX).lib \
	$(BOOST_LIBDIR)\libboost_system$(BOOST_LIB_SUFFIX).lib \
	$(BOOST_LIBDIR)\libboost_chrono$(BOOST_LIB_SUFFIX).lib

CPP = cl.exe
CPP_OPTIONS = /c /nologo /EHsc /MD /O2 /D _CRT_SECURE_NO_WARNINGS /I $(BOOST_INCLUDE) /I $(YAMI_INCLUDE)

OBJ_FILES = main.obj \
	queue-configuration.obj \
	queue-messaging.obj \
	queue-routing.obj \
	..\common\log.obj \
	..\common\properties.obj \
	..\common\utils.obj

EXEC = yami4queue.exe
    
all : $(EXEC)

$(EXEC) : $(OBJ_FILES)
	$(CPP) /Fe: $(EXEC) $(OBJ_FILES) /link $(YAMI_LIBS) $(BOOST_LIBS) $(OPEN_SSL_LIBS) Ws2_32.lib

..\common\log.obj : ..\common\log.cpp
	$(CPP) ..\common\log.cpp /Fo: ..\common\log.obj $(CPP_OPTIONS)

..\common\properties.obj : ..\common\properties.cpp
	$(CPP) ..\common\properties.cpp /Fo: ..\common\properties.obj $(CPP_OPTIONS)

..\common\utils.obj : ..\common\utils.cpp
	$(CPP) ..\common\utils.cpp /Fo: ..\common\utils.obj $(CPP_OPTIONS)

.cpp.obj::
	$(CPP) $(CPP_OPTIONS) $<

clean :
	del /Q $(EXEC) $(OBJ_FILES)
