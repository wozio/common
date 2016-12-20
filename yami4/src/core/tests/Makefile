COMPILER = c++
OPTIONS = -g -I../posix -L.. -lyamicore

# These options are used for development with OpenSSL
#OPTIONS = -g -DYAMI4_WITH_OPEN_SSL -I../posix -L.. -lyamicore -lssl

all : build test

EXECS = allocator-test parameters-test channel-test connect-test1 \
	message-test1 message-test1-hard \
	message-test2 message-test3 message-test4

build : $(EXECS)

test :
	./allocator-test
	./parameters-test
	./channel-test
	./connect-test1
	./message-test1
	./message-test1-hard
	./message-test2
	./message-test3
	./message-test4

allocator-test : allocator-test.cpp
	$(COMPILER) allocator-test.cpp -o allocator-test $(OPTIONS)

parameters-test : parameters-test.cpp
	$(COMPILER) parameters-test.cpp -o parameters-test $(OPTIONS)

channel-test : channel-test.cpp
	$(COMPILER) channel-test.cpp -o channel-test $(OPTIONS)

connect-test1 : connect-test1.cpp
	$(COMPILER) connect-test1.cpp -o connect-test1 $(OPTIONS)

message-test1 : message-test1.cpp
	$(COMPILER) message-test1.cpp -o message-test1 $(OPTIONS)

message-test1-hard : message-test1-hard.cpp
	$(COMPILER) message-test1-hard.cpp -o message-test1-hard $(OPTIONS)

message-test2 : message-test2.cpp
	$(COMPILER) message-test2.cpp -o message-test2 $(OPTIONS)

message-test3 : message-test3.cpp
	$(COMPILER) message-test3.cpp -o message-test3 $(OPTIONS)

message-test4 : message-test4.cpp
	$(COMPILER) message-test4.cpp -o message-test4 $(OPTIONS)

clean :
	rm -f $(EXECS)
