#import <yami4-objc/YAMI4.h>

#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSString.h>

#include <stdio.h>


int main(int argc, char * argv[])
{
    if (argc != 4)
    {
        puts("expecting three parameters: "
            "server destination and two integers");
        return -1;
    }

    NSAutoreleasePool * pool =
        [[NSAutoreleasePool alloc] init];
    
    NSString * serverAddress =
        [[NSString alloc] initWithUTF8String:argv[1]];

    int a = [[[NSString alloc]
        initWithUTF8String:argv[2]] intValue];
    int b = [[[NSString alloc]
        initWithUTF8String:argv[3]] intValue];
    
    @try
    {
        YAMI4Agent * clientAgent = [YAMI4Agent new];

        YAMI4Parameters * params =
            [[YAMI4Parameters alloc] init];
        [params setInteger:@"a" value:a];
        [params setInteger:@"b" value:b];

        YAMI4OutgoingMessage * message =
            [clientAgent send:serverAddress
                objectName:@"calculator"
                messageName:@"calculate"
                parameters:params];

        [message waitForCompletion];
        
        enum YAMI4MessageState state = [message state];
        if (state == YAMI4Replied)
        {
            YAMI4Parameters * reply = [message reply];

            int sum = [reply getInteger:@"sum"];
            int difference = [reply getInteger:@"difference"];
            int product = [reply getInteger:@"product"];

            int ratio;
            YAMI4ParameterEntry * ratioEntry;
            BOOL ratioDefined =
                [reply find:@"ratio" entry:&ratioEntry];
            if (ratioDefined)
            {
                ratio = [ratioEntry getInteger];
            }

            printf("sum        = %d\n", sum);
            printf("difference = %d\n", difference);
            printf("product    = %d\n", product);

            printf("ratio      = ");
            if (ratioDefined)
            {
                printf("%d\n", ratio);
            }
            else
            {
                puts("<undefined>");
            }
        }
        else if (state == YAMI4Rejected)
        {
             printf("The message has been rejected: %s\n",
                [[message exceptionMsg] UTF8String]);
        }
        else
        {
            puts("The message has been abandoned.");
        }
        
        [message close];
        [clientAgent close];
    }
    @catch (NSException * e)
    {
        printf("error: %s\n", [[e reason] UTF8String]);
    }
    
    [pool drain];
    
    return 0;
}

