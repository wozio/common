#import <yami4-objc/YAMI4.h>

#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSString.h>

#include <stdio.h>


@interface MyUpdateHandler :
    NSObject <YAMI4IncomingMessageCallback>

-(void)call:(YAMI4IncomingMessage *)message;

@end

@implementation MyUpdateHandler

-(void)call:(YAMI4IncomingMessage *)message
{
    YAMI4Parameters * content = [message parameters];

    int value = [content getInteger:@"value"];
    
    printf("received update: %d\n", value);
}

@end

int main(int argc, char * argv[])
{
    if (argc != 2)
    {
        puts("expecting one parameter: publisher destination");
        return -1;
    }

    NSAutoreleasePool * pool =
        [[NSAutoreleasePool alloc] init];
    
    NSString * publisherAddress =
        [[NSString alloc] initWithUTF8String:argv[1]];

    @try
    {
        YAMI4Agent * subscriberAgent = [YAMI4Agent new];

        // prepare subscription update callback

        NSString * updateObjectName = @"update_handler";
        
        MyUpdateHandler * updateHandler =
            [[MyUpdateHandler alloc] init];

        [subscriberAgent registerObject:updateObjectName
            callback:updateHandler];

        // subscribe to the producer

        YAMI4Parameters * params = [YAMI4Parameters new];
        [params setString:@"destination_object" 
            value:updateObjectName];

        [subscriberAgent sendOneWay:publisherAddress
            objectName:@"random_number"
            messageName:@"subscribe"
            parameters:params];

        puts("subscribed, waiting for updates");

        // block
        int dummy = getchar();
        (void) dummy;
    }
    @catch (NSException * e)
    {
        printf("error: %s\n", [[e reason] UTF8String]);
    }
    
    [pool drain];
    
    return 0;
}

