PROJECT(common)

ENABLE_LANGUAGE(CXX)

CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

SET(SOURCES src/app.cpp
            src/discovery.cpp
			src/ios_wrapper.cpp
			src/logger.cpp
			src/logger_init.cpp
			src/mcs.cpp
			src/service.cpp
			src/timer.cpp
			src/yamicontainer.cpp)
			
include_directories(../yami4/include/)
					
add_definitions(-D_ITERATOR_DEBUG_LEVEL=2 -DPOCO_STATIC -DDISABLE_LOGS)

ADD_LIBRARY(common STATIC ${SOURCES})