#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Environment
MKDIR=mkdir
CP=cp
GREP=grep
NM=nm
CCADMIN=CCadmin
RANLIB=ranlib
CC=gcc
CCC=g++
CXX=g++
FC=gfortran
AS=as

# Macros
CND_PLATFORM=GNU-Linux-x86
CND_DLIB_EXT=so
CND_CONF=Debug
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/src/app.o \
	${OBJECTDIR}/src/discovery.o \
	${OBJECTDIR}/src/ios_wrapper.o \
	${OBJECTDIR}/src/logger.o \
	${OBJECTDIR}/src/mcs.o \
	${OBJECTDIR}/src/service.o \
	${OBJECTDIR}/src/timer.o \
	${OBJECTDIR}/src/yamicontainer.o


# C Compiler Flags
CFLAGS=

# CC Compiler Flags
CCFLAGS=
CXXFLAGS=

# Fortran Compiler Flags
FFLAGS=

# Assembler Flags
ASFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../${CND_CONF}/libcommon.a

../${CND_CONF}/libcommon.a: ${OBJECTFILES}
	${MKDIR} -p ../${CND_CONF}
	${RM} ../${CND_CONF}/libcommon.a
	${AR} -rv ../${CND_CONF}/libcommon.a ${OBJECTFILES} 
	$(RANLIB) ../${CND_CONF}/libcommon.a

${OBJECTDIR}/src/app.o: src/app.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/app.o src/app.cpp

${OBJECTDIR}/src/discovery.o: src/discovery.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/discovery.o src/discovery.cpp

${OBJECTDIR}/src/ios_wrapper.o: src/ios_wrapper.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/ios_wrapper.o src/ios_wrapper.cpp

${OBJECTDIR}/src/logger.o: src/logger.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/logger.o src/logger.cpp

${OBJECTDIR}/src/mcs.o: src/mcs.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/mcs.o src/mcs.cpp

${OBJECTDIR}/src/service.o: src/service.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/service.o src/service.cpp

${OBJECTDIR}/src/timer.o: src/timer.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/timer.o src/timer.cpp

${OBJECTDIR}/src/yamicontainer.o: src/yamicontainer.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/yamicontainer.o src/yamicontainer.cpp

# Subprojects
.build-subprojects:
	cd ../yami4/src/core && ${MAKE} -f Makefile
	cd ../yami4/src/cpp && ${MAKE} -f Makefile

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../${CND_CONF}/libcommon.a

# Subprojects
.clean-subprojects:
	cd ../yami4/src/core && ${MAKE} -f Makefile clean
	cd ../yami4/src/cpp && ${MAKE} -f Makefile clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
