// Copyright Maciej Sobczak 2008-2014.
// This file is part of YAMI4.
//
// YAMI4 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// YAMI4 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

#import <yami4-objc/Agent.h>
#import <yami4-objc/Exception.h>
#import <yami4-objc/Parameters.h>

#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSThread.h>
#include <assert.h>
#include <stdlib.h>


void test1()
{
    YAMI4Agent * clientAgent = [YAMI4Agent new];
    
    @try
    {
        [clientAgent sendOneWay:@"tcp://nosuchaddress:12345"
            objectName:@"nosuchobject" messageName:@"badmessage"];
        
        assert(false);
    }
    @catch (YAMI4Exception * e)
    {
        assert([[e reason] isEqualToString:@"I/O error."]);
    }

    @try
    {
        // a bit dodgy, but 4 is an unassigned port in the list
        // of well-known services, so there is a chance that
        // no existing process uses it on the machine where this test
        // is executed
        // - if this test fails then it is a sign that some process
        // has a listening socket on port 4 - pick another dummy number
        
        [clientAgent sendOneWay:@"tcp://localhost:4"
            objectName:@"nosuchobject" messageName:@"badmessage"];

        assert(false);
    }
    @catch (YAMI4Exception * e)
    {
        assert([[e reason] isEqualToString:@"I/O error."]);
    }
    
    [clientAgent close];
}

// message sent to nonexisting object
void test2()
{
    YAMI4Agent * serverAgent = [YAMI4Agent new];

    NSString * serverAddress = [serverAgent addListener:@"tcp://*:*"];

    YAMI4Agent * clientAgent = [YAMI4Agent new];

    // one-way message does not report any error
    [clientAgent sendOneWay:serverAddress
        objectName:@"nosuchobject" messageName:@"badmessage"];

    // two-way message is rejected
    YAMI4OutgoingMessage * message =
        [clientAgent send:serverAddress
            objectName:@"nosuchobject" messageName:@"badmessage"];

    [message waitForCompletion];

    assert([message state] == YAMI4Rejected);
    assert([[message exceptionMsg]
        isEqualToString:@"Unknown destination object."]);

    [clientAgent close];
    [serverAgent close];
}

// message sent to nonexisting object, explicit connection management
void test2a()
{
    YAMI4Agent * serverAgent = [YAMI4Agent new];

    NSString * serverAddress = [serverAgent addListener:@"tcp://*:*"];

    YAMI4Agent * clientAgent = [YAMI4Agent new];
    
    size_t priority = 0;
    BOOL autoConnect = NO;

    // message fails if there is no channel and no auto-connect
    @try
    {
        YAMI4OutgoingMessage * message =
            [clientAgent send:serverAddress
                objectName:@"nosuchobject" messageName:@"badmessage"
                parameters:nil priority:priority autoConnect:autoConnect];

        // dummy, otherwise it is a statically unused variable
        assert(message == nil);
        
        assert(false);
    }
    @catch (YAMI4Exception * e)
    {
        assert([[e reason] isEqualToString:@"I/O error."]);
    }

    // explicitly open the channel

    [clientAgent openConnection:serverAddress];

    // message is successfully sent over existing channel,
    // but later rejected by server

    YAMI4OutgoingMessage * message =
        [clientAgent send:serverAddress
            objectName:@"nosuchobject" messageName:@"badmessage"
            parameters:nil priority:0 autoConnect:autoConnect];

    [message waitForCompletion];

    assert([message state] == YAMI4Rejected);
    assert([[message exceptionMsg]
        isEqualToString:@"Unknown destination object."]);

    [clientAgent close];
    [serverAgent close];
}

@interface ObjectTypeForTest3 : NSObject <YAMI4IncomingMessageCallback>
{
@public
    BOOL gotMessage_;
}

-(ObjectTypeForTest3 *)init;
-(void)call:(YAMI4IncomingMessage *)message;

@end

@implementation ObjectTypeForTest3

-(ObjectTypeForTest3 *)init
{
    self = [super init];
    if (self != nil)
    {
        gotMessage_ = NO;
    }
    
    return self;
}

-(void)call:(YAMI4IncomingMessage *)message;
{
    gotMessage_ = YES;

    assert([[message objectName] isEqualToString:@"object"]);
    assert([[message messageName] isEqualToString:@"message"]);

    YAMI4Parameters * content =
        [[YAMI4Parameters alloc] initWithParameters:[message parameters]];
    assert([content size] == 1);
    assert([[content getString:@"value"] isEqualToString:@"ping"]);

    [content setString:@"value" value:@"pong"];
    [message reply:content];
}

@end

// message sent and replied to
void test3()
{
    YAMI4Agent * serverAgent = [YAMI4Agent new];

    NSString * serverAddress = [serverAgent addListener:@"tcp://*:*"];

    ObjectTypeForTest3 * myObject = [[ObjectTypeForTest3 alloc] init];
    [serverAgent registerObject:@"object" callback:myObject];

    YAMI4Agent * clientAgent = [YAMI4Agent new];

    YAMI4Parameters * content = [[YAMI4Parameters alloc] init];
    [content setString:@"value" value:@"ping"];

    YAMI4OutgoingMessage * message =
        [clientAgent send:serverAddress
            objectName:@"object" messageName:@"message" parameters:content];
    
    [message waitForTransmission];

    // after transmission the whole message is pushed out
    size_t sentBytes;
    size_t totalByteCount;
    enum YAMI4MessageState state =
        [message state:&sentBytes totalByteCount:&totalByteCount];
    (void) state;

    assert(sentBytes == totalByteCount);

    [message waitForCompletion];

    assert(myObject->gotMessage_);

    assert([message state] == YAMI4Replied);

    content = [message reply];
    assert([[content getString:@"value"] isEqualToString:@"pong"]);

    [message close];
    [clientAgent close];
    [serverAgent close];
}

@interface ObjectTypeForTest4 : NSObject <YAMI4IncomingMessageCallback>
{
@public
    BOOL gotMessage_;
}

-(ObjectTypeForTest4 *)init;
-(void)call:(YAMI4IncomingMessage *)message;

@end

@implementation ObjectTypeForTest4

-(ObjectTypeForTest4 *)init
{
    self = [super init];
    if (self != nil)
    {
        gotMessage_ = NO;
    }
    
    return self;
}

-(void)call:(YAMI4IncomingMessage *)message;
{
    gotMessage_ = YES;

    // expect empty parameters if no content is sent

    YAMI4Parameters * content = [message parameters];
    assert([content size] == 0);

    [message reject:@"some reason"];
}

@end

// message rejected by server
void test4()
{
    YAMI4Agent * serverAgent = [YAMI4Agent new];

    NSString * serverAddress = [serverAgent addListener:@"tcp://*:*"];

    ObjectTypeForTest4 * myObject = [[ObjectTypeForTest4 alloc] init];
    [serverAgent registerObject:@"object" callback:myObject];

    YAMI4Agent * clientAgent = [YAMI4Agent new];

    YAMI4OutgoingMessage * message =
        [clientAgent send:serverAddress
            objectName:@"object" messageName:@"message"];
    
    [message waitForCompletion];

    assert(myObject->gotMessage_);

    assert([message state] == YAMI4Rejected);
    assert([[message exceptionMsg] isEqualToString:@"some reason"]);

    [message close];
    [clientAgent close];
    [serverAgent close];
}

@interface ObjectTypeForTest5 : NSObject <YAMI4IncomingMessageCallback>
{
@public
    BOOL gotMessage_;
}

-(ObjectTypeForTest5 *)init;
-(void)call:(YAMI4IncomingMessage *)message;

@end

@implementation ObjectTypeForTest5

-(ObjectTypeForTest5 *)init
{
    self = [super init];
    if (self != nil)
    {
        gotMessage_ = NO;
    }
    
    return self;
}

-(void)call:(YAMI4IncomingMessage *)message;
{
    gotMessage_ = YES;

    @throw [[NSException alloc] initWithName:@"MyException"
        reason:@"something bad happened" userInfo:nil];
}

@end

// message rejected due to exception in user code at the server side
void test5()
{
    YAMI4Agent * serverAgent = [YAMI4Agent new];

    NSString * serverAddress = [serverAgent addListener:@"tcp://*:*"];

    ObjectTypeForTest5 * myObject = [[ObjectTypeForTest5 alloc] init];
    [serverAgent registerObject:@"object" callback:myObject];

    YAMI4Agent * clientAgent = [YAMI4Agent new];

    YAMI4OutgoingMessage * message =
        [clientAgent send:serverAddress
            objectName:@"object" messageName:@"message"];
    
    [message waitForCompletion];

    assert(myObject->gotMessage_);

    assert([message state] == YAMI4Rejected);
    assert([[message exceptionMsg]
        hasSuffix:@"something bad happened"]);

    [message close];
    [clientAgent close];
    [serverAgent close];
}

#define numOfMessagesInTest6 10

@interface ObjectTypeForTest6 : NSObject <YAMI4IncomingMessageCallback>
{
@public
    NSString * bigString_;
    BOOL gotMessages_[numOfMessagesInTest6];
}

-(ObjectTypeForTest6 *)initWithBigString:(NSString *)bigString;
-(void)call:(YAMI4IncomingMessage *)message;

@end

@implementation ObjectTypeForTest6

-(ObjectTypeForTest6 *)initWithBigString:(NSString *)bigString
{
    self = [super init];
    if (self != nil)
    {
        bigString_ = bigString;

        size_t i;
        for (i = 0; i != numOfMessagesInTest6; ++i)
        {
            gotMessages_[i] = NO;
        }
    }
    
    return self;
}

-(void)call:(YAMI4IncomingMessage *)message;
{
    YAMI4Parameters * content = [message parameters];
    int id = [content getInteger:@"id"];

    gotMessages_[id] = YES;

    // verify the big value
    NSString * value = [content getString:@"big"];
    assert([bigString_ isEqualToString:value]);

    [message reply];
}

@end

// big messages sent with different priorities
void test6()
{
    // Note:
    // The messages are sent with different priorities, which means
    // that they might complete in the order that is different from the
    // order of posting them to the outgoing queue.
    // The messages are posted with increasing priorities (first message
    // is sent with lowest priority, last message with highest),
    // so it is *very likely* that they will be received by server
    // in the reversed order, but this cannot be guaranteed as there is
    // no relation between the speed of posting and the speed
    // of transmission.

    const size_t sizeOfBigString = 1000000;
    char * bigBuf = (char *)malloc(sizeOfBigString + 1);
    size_t i;
    for (i = 0; i != sizeOfBigString; ++i)
    {
        bigBuf[i] = 'x';
    }
    bigBuf[sizeOfBigString] = '\0';

    NSString * bigString = [[NSString alloc] initWithUTF8String:bigBuf];

    YAMI4Agent * serverAgent = [YAMI4Agent new];

    NSString * serverAddress = [serverAgent addListener:@"tcp://*:*"];

    ObjectTypeForTest6 * myObject =
        [[ObjectTypeForTest6 alloc] initWithBigString:bigString];

    [serverAgent registerObject:@"object" callback:myObject];

    YAMI4Agent * clientAgent = [YAMI4Agent new];

    YAMI4Parameters * content = [YAMI4Parameters new];
    [content setString:@"big" value:bigString];

    YAMI4OutgoingMessage * messages[numOfMessagesInTest6];

    for (i = 0; i != numOfMessagesInTest6; ++i)
    {
        int id = (int)i;
        size_t priority = i;

        [content setInteger:@"id" value:id];

        messages[i] =
            [clientAgent send:serverAddress
                objectName:@"object" messageName:@"message"
                     parameters:content priority:priority];
    }
    
    // wait for all messages to complete
    for (i = 0; i != numOfMessagesInTest6; ++i)
    {
        [messages[i] waitForCompletion];
    }

    for (i = 0; i != numOfMessagesInTest6; ++i)
    {
        assert(myObject->gotMessages_[i]);

        [messages[i] close];
    }

    [clientAgent close];
    [serverAgent close];
}

@interface ObjectTypeForTests78 : NSObject <YAMI4IncomingMessageCallback>
{
@public
    BOOL gotMessage_;
}

-(ObjectTypeForTests78 *)init;
-(void)call:(YAMI4IncomingMessage *)message;

@end

@implementation ObjectTypeForTests78

-(ObjectTypeForTests78 *)init
{
    self = [super init];
    if (self != nil)
    {
        gotMessage_= NO;
    }
    
    return self;
}

-(void)call:(YAMI4IncomingMessage *)message;
{
    gotMessage_ = YES;

    [message reply];
}

@end

// message sent to load-balanced pair of destinations
void test7()
{
    YAMI4Agent * serverAgent1 = [YAMI4Agent new];

    NSString * serverAddress1 = [serverAgent1 addListener:@"tcp://*:*"];

    YAMI4Agent * serverAgent2 = [YAMI4Agent new];

    NSString * serverAddress2 = [serverAgent2 addListener:@"tcp://*:*"];
    
    NSString * loadBalancedTarget =
        [[NSString alloc] initWithFormat:@"failover:(%@|%@)",
            serverAddress1, serverAddress2];

    ObjectTypeForTests78 * myObject1 = [[ObjectTypeForTests78 alloc] init];
    [serverAgent1 registerObject:@"object" callback:myObject1];

    ObjectTypeForTests78 * myObject2 = [[ObjectTypeForTests78 alloc] init];
    [serverAgent2 registerObject:@"object" callback:myObject2];

    YAMI4Agent * clientAgent = [YAMI4Agent new];

    YAMI4OutgoingMessage * message =
        [clientAgent send:loadBalancedTarget
            objectName:@"object" messageName:@"message"];
    
    // since this is a load-balanced (and failover) target,
    // the message is implicitly waited for completion

    assert([message state] == YAMI4Replied);

    // exactly one of two servers got the message
    assert((myObject1->gotMessage_ && myObject2->gotMessage_ == NO) ||
        (myObject1->gotMessage_ == NO && myObject2->gotMessage_));

    [message close];
    [clientAgent close];
    [serverAgent1 close];
    [serverAgent2 close];
}

// message sent to failover pair of destinations
void test8()
{
    // the failover pair consists of one proper address and one
    // that is certainly not working

    YAMI4Agent * serverAgent = [YAMI4Agent new];
    NSString * serverAddress = [serverAgent addListener:@"tcp://*:*"];

    ObjectTypeForTests78 * myObject = [[ObjectTypeForTests78 alloc] init];
    [serverAgent registerObject:@"object" callback:myObject];

    YAMI4Agent * clientAgent = [YAMI4Agent new];

    NSString * brokenTarget = @"tcp://nosuchhost:4";

    NSString * failOverTarget =
        [[NSString alloc] initWithFormat:@"failover:(%@|%@)",
            serverAddress, brokenTarget];

    YAMI4OutgoingMessage * message =
        [clientAgent send:failOverTarget
            objectName:@"object" messageName:@"message"];
    
    // since this is a load-balanced (and failover) target,
    // the message is implicitly waited for completion

    assert([message state] == YAMI4Replied);

    // the working server in the failover pair got the message
    assert(myObject->gotMessage_);

    [message close];
    [clientAgent close];
    [serverAgent close];
}

// empty failover group is an error
void test9()
{
    YAMI4Agent * clientAgent = [YAMI4Agent new];

    @try
    {
        [clientAgent sendOneWay:@"failover:()"
            objectName:@"object" messageName:@"message"];

        assert(false);
    }
    @catch (NSException * e)
    {
        assert([[e reason]
            isEqualToString:@"Empty failover group is not allowed."]);
    }

    [clientAgent close];
}

// test10 - raw binary messages are not supported

// test11 - connection event notifications are not supported

// test12 - frame size border conditions - no need to test it here
// as the serialization is done (and tested) at the C++ level

int main()
{
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

    test1();
    test2();
    test2a();
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
    test9();

    [pool drain];
    return 0;
}
