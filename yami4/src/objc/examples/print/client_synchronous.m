#import <yami4-objc/YAMI4.h>

#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSString.h>

#include <stdio.h>
#include <string.h>


#define MAX_LINE_LENGTH 100

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
        YAMI4Agent * clientAgent = [YAMI4Agent new];

        // read lines of text from standard input
        // and post each one for transmission

        while (YES)
        {
            char lineBuf[MAX_LINE_LENGTH];
            char * dummy =
                fgets(lineBuf, MAX_LINE_LENGTH, stdin);
            (void) dummy;
            if (feof(stdin) != 0)
            {
                break;
            }
            
            // clear newlines
            lineBuf[strlen(lineBuf) - 1] = '\0';
            
            NSString * inputLine =
                [[NSString alloc] initWithUTF8String:lineBuf];

            YAMI4Parameters * params =
                [[YAMI4Parameters alloc] init];
            
            // the "content" field name is arbitrary,
            // but needs to be recognized at the server side

            [params setString:@"content" value:inputLine];

            YAMI4OutgoingMessage * message =
                [clientAgent send:serverAddress
                    objectName:@"printer" messageName:@"print"
                    parameters:params];
                    
            [message waitForTransmission];
            
            [message close];
        }
        
        [clientAgent close];
    }
    @catch (NSException * e)
    {
        printf("error: %s\n", [[e reason] UTF8String]);
    }
    
    [pool drain];
    
    return 0;
}

