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

#import "Agent.h"
#import "Exception.h"
#import "Parameters.h"
#import "ValuePublisher.h"

#include <yami4-cpp/agent.h>
#include <yami4-cpp/parameters.h>
#include <yami4-cpp/value_publisher.h>


@implementation YAMI4ValuePublisher

+(YAMI4ValuePublisher *)new
{
    YAMI4ValuePublisher * newObject = [[YAMI4ValuePublisher alloc] init];
    return newObject;
}

-(YAMI4ValuePublisher *)init
{
    self = [super init];
    if (self != nil)
    {
        vp_ = new yami::value_publisher();
    }

    return self;
}

-(void)dealloc
{
    [self close];

    [super dealloc];
}

-(void)close
{
    if (vp_ != NULL)
    {
        yami::value_publisher * vp =
            static_cast<yami::value_publisher *>(vp_);
        delete vp;
        
        vp_ = NULL;
    }
}

-(void)registerAt:(YAMI4Agent *)controllingAgent
    objectName:(NSString *)objectName
{
    yami::value_publisher * vp = static_cast<yami::value_publisher *>(vp_);
    
    yami::agent * a =
        static_cast<yami::agent *>([controllingAgent getInternals]);
    
    try
    {
        vp->register_at(*a, [objectName UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)unregister
{
    yami::value_publisher * vp = static_cast<yami::value_publisher *>(vp_);
    
    try
    {
        vp->unregister();
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)subscribe:(NSString *)destinationTarget
    destinationObject:(NSString *)destinationObject
{
    yami::value_publisher * vp = static_cast<yami::value_publisher *>(vp_);
    
    try
    {
        vp->subscribe([destinationTarget UTF8String],
            [destinationObject UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)unsubscribe:(NSString *)destinationTarget
{
    yami::value_publisher * vp = static_cast<yami::value_publisher *>(vp_);
    
    try
    {
        vp->unsubscribe([destinationTarget UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)publish:(YAMI4Parameters *)parameters
{
    [self publish:parameters priority:0];
}

-(void)publish:(YAMI4Parameters *)parameters
    priority:(size_t)priority
{
    yami::value_publisher * vp = static_cast<yami::value_publisher *>(vp_);
    
    try
    {
        if (parameters != nil)
        {
            yami::parameters * params =
                static_cast<yami::parameters *>([parameters getInternals]);

            vp->publish(*params, priority);
        }
        else
        {
            vp->publish(yami::parameters(), priority);
        }
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(size_t)numberOfSubscribers
{
    yami::value_publisher * vp = static_cast<yami::value_publisher *>(vp_);
    
    try
    {
        return vp->get_number_of_subscribers();
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return 0;
}

-(struct YAMI4SubscriberInfo *)subscribers:(size_t*)numOfSubscribers
{
    yami::value_publisher * vp = static_cast<yami::value_publisher *>(vp_);
    
    try
    {
        std::vector<std::pair<std::string, std::string> > subs(
            vp->get_subscribers());

        std::size_t size = subs.size();
        
        *numOfSubscribers = size;
        
        if (subs.empty())
        {
            return NULL;
        }
        
        struct YAMI4SubscriberInfo * ret =
            (struct YAMI4SubscriberInfo *)
                malloc(size * sizeof(YAMI4SubscriberInfo));

        for (size_t i = 0; i != size; ++i)
        {
            ret[i].destinationTarget =
                [[NSString alloc]
                    initWithUTF8String:subs[i].first.c_str()];

            ret[i].destinationObject =
                [[NSString alloc]
                    initWithUTF8String:subs[i].second.c_str()];
        }
        
        return ret;
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return NULL;
}

@end

