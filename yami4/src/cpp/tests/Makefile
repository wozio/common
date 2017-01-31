COMPILER = c++
OPTIONS = -g

INCLUDES = -I../../../include
LIBS = -L../../../lib -lyamicpp -lyamicore -lpthread

all : build test

EXECS = parameters-test agent-test subscription-test

build : $(EXECS)

test :
	./parameters-test
	./agent-test
	./subscription-test

parameters-test : parameters-test.cpp
	$(COMPILER) parameters-test.cpp -o parameters-test \
		$(OPTIONS) $(INCLUDES) $(LIBS)

agent-test : agent-test.cpp
	$(COMPILER) agent-test.cpp -o agent-test \
		$(OPTIONS) $(INCLUDES) $(LIBS)

subscription-test : subscription-test.cpp
	$(COMPILER) subscription-test.cpp -o subscription-test \
		$(OPTIONS) $(INCLUDES) $(LIBS)

# optional test
notifications-test : notifications-test.cpp
	$(COMPILER) notifications-test.cpp -o notifications-test \
		$(OPTIONS) $(INCLUDES) $(LIBS)


clean :
	rm -f $(EXECS)
