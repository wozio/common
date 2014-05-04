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

#import "Exception.h"


@implementation YAMI4Exception

+(void)raiseFrom:(const char *)msg
{
    YAMI4Exception * newException = [YAMI4Exception alloc];
    newException = [newException initWithMsg:msg];

    [newException raise];
}

-(YAMI4Exception *)initWithMsg:(const char *)msg
{
    NSString * nsMsg = [[NSString alloc] initWithUTF8String:msg];
    self = [super initWithName:@"YAMI4Exception" reason:nsMsg userInfo:nil];

    [nsMsg release];

    return self;
}

@end
