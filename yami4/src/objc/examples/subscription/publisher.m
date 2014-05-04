#import <yami4-objc/YAMI4.h>

#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSString.h>
#import <Foundation/NSThread.h>

#include <stdio.h>
#include <stdlib.h>


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
        YAMI4ValuePublisher * randomValue =
            [YAMI4ValuePublisher new];
        
        YAMI4Agent * publisherAgent = [YAMI4Agent new];

        NSString * resolvedAddress =
            [publisherAgent addListener:publisherAddress];

        printf("The publisher is listening on %s\n",
            [resolvedAddress UTF8String]);
        
        [publisherAgent registerValuePublisher:@"random_number"
            publisher:randomValue];

        // publish random numbers forever
        YAMI4Parameters * content = [YAMI4Parameters new];
        while (YES)
        {
            int random = rand() % 100;
            [content setInteger:@"value" value:random];
            
            printf("publishing value %d\n", random);
            
            [randomValue publish:content];
            
            // pause for 1s
            [NSThread sleepForTimeInterval:1.0];
        }
    }
    @catch (NSException * e)
    {
        printf("error: %s\n", [[e reason] UTF8String]);
    }
    
    [pool drain];
    
    return 0;
}

