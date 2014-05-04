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

    int a = [params getInteger:@"a"];
    int b = [params getInteger:@"b"];

    // prepare the answer with results of four calculations

    YAMI4Parameters * replyParams = [YAMI4Parameters new];

    [replyParams setInteger:@"sum" value:(a + b)];
    [replyParams setInteger:@"difference" value:(a - b)];
    [replyParams setInteger:@"product" value:(a * b)];

    // if the ratio cannot be computed,
    // it is not included in the response
    // the client will interpret that fact properly
    if (b != 0)
    {
        [replyParams setInteger:@"ratio" value:(a / b)];
    }

    [message reply:replyParams];

    printf("got message with parameters %d and %d, "
        "response has been sent back\n", a, b);
}

@end

int main(int argc, char * argv[])
{
    if (argc != 2)
    {
        puts("expecting one parameter: name server address");
        return -1;
    }

    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    
    NSString * nameServerAddress =
        [[NSString alloc] initWithUTF8String:argv[1]];

    @try
    {
        YAMI4Agent * serverAgent = [YAMI4Agent new];

        // prepare the server and bind its address
        // to the name server

        NSString * resolvedAddress =
            [serverAgent addListener:@"tcp://*:*"];

        printf("The server is listening on %s\n",
            [resolvedAddress UTF8String]);
            
        YAMI4Parameters * bindParams = [[YAMI4Parameters alloc] init];
        [bindParams setString:@"object" value:@"calculator"];
        [bindParams setString:@"location" value:resolvedAddress];

        YAMI4OutgoingMessage * nsBind =
            [serverAgent send:nameServerAddress
                objectName:@"names" messageName:@"bind"
                parameters:bindParams];

        [nsBind waitForCompletion];
 
        if ([nsBind state] != YAMI4Replied)
        {
            printf("error: %s\n", [[nsBind exceptionMsg] UTF8String]);

            return -1;
        }

        puts("Address bound by name server.");

        MyHandler * myObject = [[MyHandler alloc] init];
        [serverAgent registerObject:@"calculator" callback:myObject];

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

