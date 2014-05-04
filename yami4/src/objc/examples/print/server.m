#import <yami4-objc/YAMI4.h>

#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSString.h>

#include <stdio.h>


@interface MyHandler : NSObject <YAMI4IncomingMessageCallback>

-(void)call:(YAMI4IncomingMessage *)message;

@end

@implementation MyHandler

-(void)call:(YAMI4IncomingMessage *)message
{
    YAMI4Parameters * params = [message parameters];

    // extract the content field
    // and print it on standard output

    NSString * line = [params getString:@"content"];
    printf("%s\n", [line UTF8String]);
}

@end

int main(int argc, char * argv[])
{
    if (argc != 2)
    {
        puts("expecting one parameter: server destination");
        return -1;
    }

    NSAutoreleasePool * pool =
        [[NSAutoreleasePool alloc] init];
    
    NSString * serverAddress =
        [[NSString alloc] initWithUTF8String:argv[1]];

    @try
    {
        YAMI4Agent * serverAgent = [YAMI4Agent new];

        NSString * resolvedAddress =
            [serverAgent addListener:serverAddress];

        printf("The server is listening on %s\n",
            [resolvedAddress UTF8String]);

        MyHandler * myObject = [[MyHandler alloc] init];
        [serverAgent registerObject:@"printer"
            callback:myObject];

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

