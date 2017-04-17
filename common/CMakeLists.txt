file(GLOB_RECURSE SRCS src/*.cpp)
add_library(common STATIC ${SRCS})