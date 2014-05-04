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
#import <yami4-objc/ValuePublisher.h>

#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSThread.h>
#include <assert.h>
#include <stdlib.h>


// Note: this test cannot be easily implemented "properly",
// because it also checks that the message was *not* received.
// This is done with artificial sleep calls with the assumption
// that the interaction between agents awlays takes place
// within the period of 1s.
// Since the value publisher feature does not involve any
// system-level services and is entirely implemented in terms of
// YAMI4 API, a POSIX version of this test is sufficient.

@interface ObjectTypeForTest1 : NSObject <YAMI4IncomingMessageCallback>
{
@public
    BOOL gotUpdate_;
}

-(ObjectTypeForTest1 *)init;
-(void)call:(YAMI4IncomingMessage *)message;

@end

@implementation ObjectTypeForTest1

-(ObjectTypeForTest1 *)init
{
    self = [super init];
    if (self != nil)
    {
        gotUpdate_ = NO;
    }
    
    return self;
}

-(void)call:(YAMI4IncomingMessage *)message;
{
    assert([[message messageName] isEqualToString:@"subscription_update"]);
    gotUpdate_ = YES;
}

@end

void test1()
{
    // set up the publisher side

    YAMI4Agent * publisherAgent = [YAMI4Agent new];
    
    NSString * publisherAddress = [publisherAgent addListener:@"tcp://*:*"];
    
    YAMI4ValuePublisher * value = [YAMI4ValuePublisher new];
    [publisherAgent registerValuePublisher:@"my_value" publisher:value];

    // no subscribers yet

    assert([value numberOfSubscribers] == 0);
    size_t numOfSubs;
    assert([value subscribers:&numOfSubs] == NULL);
    assert(numOfSubs == 0);

    // set up the subscriber side

    ObjectTypeForTest1 * myUpdateHandler = [[ObjectTypeForTest1 alloc] init];

    YAMI4Agent * subscriberAgent = [YAMI4Agent new];
    [subscriberAgent registerObject:@"my_update_handler"
        callback:myUpdateHandler];

    // subscribe

    YAMI4Parameters * params = [YAMI4Parameters new];
    [params setString:@"destination_object" value:@"my_update_handler"];

    YAMI4OutgoingMessage * subscribeMessage =
        [subscriberAgent send:publisherAddress
            objectName:@"my_value" messageName:@"subscribe"
            parameters:params];

    [subscribeMessage waitForCompletion];
    
    // there should be one subscriber, as seen at the publisher side

    assert([value numberOfSubscribers] == 1);
    struct YAMI4SubscriberInfo * subscribers =
        [value subscribers:&numOfSubs];
    assert(subscribers != NULL);
    assert(numOfSubs == 1);
    assert([subscribers[0].destinationObject
        isEqualToString:@"my_update_handler"]);
    
    free(subscribers);
    
    // publish some value

    YAMI4Parameters * dummy = [YAMI4Parameters new];
    [value publish:dummy];

    // check if the listener got it

    [NSThread sleepForTimeInterval:1.0];

    assert(myUpdateHandler->gotUpdate_);

    // unsubscribe

    YAMI4OutgoingMessage * unsubscribeMessage =
        [subscriberAgent send:publisherAddress
            objectName:@"my_value" messageName:@"unsubscribe"];

    [unsubscribeMessage waitForCompletion];
    
    // there should be no subscribers

    assert([value numberOfSubscribers] == 0);
    assert([value subscribers:&numOfSubs] == NULL);
    assert(numOfSubs == 0);

    // check that the updates do not arrive any longer

    myUpdateHandler->gotUpdate_ = NO;
    [value publish:dummy];

    [NSThread sleepForTimeInterval:1.0];
    assert(myUpdateHandler->gotUpdate_ == NO);

    [value close];
    [subscriberAgent close];
    [publisherAgent close];
}

// test2 - notifications of unknown commands are not supported

int main()
{
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

    test1();

    [pool drain];
    return 0;
}
