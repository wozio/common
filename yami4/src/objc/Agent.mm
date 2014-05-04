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
#include <yami4-cpp/errors.h>
#include <yami4-cpp/incoming_message.h>
#include <yami4-cpp/outgoing_message.h>
#include <yami4-cpp/parameters.h>

#import <Foundation/NSAutoreleasePool.h>

#include <cassert>


// helper dispatcher for incoming messages
void incomingMessageDispatcher(yami::incoming_message & im, void * hint)
{
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

    id <YAMI4IncomingMessageCallback> callback =
        static_cast<id <YAMI4IncomingMessageCallback> >(hint);

    YAMI4IncomingMessage * msg = [YAMI4IncomingMessage alloc];
    msg = [msg initWithObject:&im];

    @try
    {
        [callback call:msg];

        [pool drain];
    }
    @catch (NSException * e)
    {
        const std::string reason = [[e reason] UTF8String];

        [pool drain];

        throw yami::yami_runtime_error(reason);
    }
}

@implementation YAMI4Agent

+(YAMI4Agent *)new
{
    YAMI4Agent * newObject = [[YAMI4Agent alloc] init];
    return newObject;
}

-(YAMI4Agent *)init
{
    self = [super init];
    if (self != nil)
    {
        agent_ = new yami::agent();
    }

    return self;
}

-(YAMI4Agent *)initWithOptions:(YAMI4Parameters *)options
{
    if (options == nil)
    {
        return [self init];
    }

    yami::parameters * params =
        static_cast<yami::parameters *>([options getInternals]);

    self = [super init];
    if (self != nil)
    {
        agent_ = new yami::agent(*params);
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
    if (agent_ != NULL)
    {
        yami::agent * a = static_cast<yami::agent *>(agent_);
        delete a;
        
        agent_ = NULL;
    }
}

-(NSString *)addListener:(NSString *)listener
{
    yami::agent * a = static_cast<yami::agent *>(agent_);

    try
    {
        const std::string & addedListener =
            a->add_listener([listener UTF8String]);

        NSString * ret =
            [[NSString alloc] initWithUTF8String:addedListener.c_str()];
        
        return ret;
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return nil;
}

-(void)removeListener:(NSString *)listener
{
    yami::agent * a = static_cast<yami::agent *>(agent_);

    try
    {
        a->remove_listener([listener UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)registerObject:(NSString *)objectName
    callback:(id <YAMI4IncomingMessageCallback>)callback
{
    yami::agent * a = static_cast<yami::agent *>(agent_);

    try
    {
        a->register_raw_object([objectName UTF8String],
            &incomingMessageDispatcher, callback);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)unregisterObject:(NSString *)objectName
{
    yami::agent * a = static_cast<yami::agent *>(agent_);

    try
    {
        a->unregister_object([objectName UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)registerValuePublisher:(NSString *)objectName
    publisher:(YAMI4ValuePublisher *)publisher
{
    [publisher registerAt:self objectName:objectName];
}

-(void)openConnection:(NSString *)target
{
    yami::agent * a = static_cast<yami::agent *>(agent_);

    try
    {
        a->open_connection([target UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)openConnection:(NSString *)target
    options:(YAMI4Parameters *)options
{
    if (options == nil)
    {
        [self openConnection:target];
        return;
    }

    yami::agent * a = static_cast<yami::agent *>(agent_);

    yami::parameters * params =
        static_cast<yami::parameters *>([options getInternals]);

    try
    {
        a->open_connection([target UTF8String], *params);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)sendOneWay:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
{
    [self sendOneWay:target objectName:objectName messageName:messageName
        parameters:nil priority:0 autoConnect:YES];
}

-(void)sendOneWay:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
    parameters:(YAMI4Parameters *)parameters
{
    [self sendOneWay:target objectName:objectName messageName:messageName
        parameters:parameters priority:0 autoConnect:YES];
}

-(void)sendOneWay:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
    parameters:(YAMI4Parameters *)parameters
    priority:(size_t)priority
{
    [self sendOneWay:target objectName:objectName messageName:messageName
        parameters:parameters priority:priority autoConnect:YES];
}

-(void)sendOneWay:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
    parameters:(YAMI4Parameters *)parameters
    priority:(size_t)priority
    autoConnect:(BOOL)autoConnect
{
    yami::agent * a = static_cast<yami::agent *>(agent_);

    try
    {
        if (parameters != nil)
        {
            yami::parameters * params =
                static_cast<yami::parameters *>([parameters getInternals]);
                
            a->send_one_way([target UTF8String],
                [objectName UTF8String], [messageName UTF8String],
                *params, priority, autoConnect);
        }
        else
        {
            a->send_one_way([target UTF8String],
                [objectName UTF8String], [messageName UTF8String],
                yami::parameters(), priority, autoConnect);
        }
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(YAMI4OutgoingMessage *)send:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
{
    return [self send:target objectName:objectName messageName:messageName
        parameters:nil priority:0 autoConnect:YES];
}

-(YAMI4OutgoingMessage *)send:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
    parameters:(YAMI4Parameters *)parameters
{
    return [self send:target objectName:objectName messageName:messageName
        parameters:parameters priority:0 autoConnect:YES];
}

-(YAMI4OutgoingMessage *)send:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
    parameters:(YAMI4Parameters *)parameters
    priority:(size_t)priority
{
    return [self send:target objectName:objectName messageName:messageName
        parameters:parameters priority:priority autoConnect:YES];
}

-(YAMI4OutgoingMessage *)send:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
    parameters:(YAMI4Parameters *)parameters
    priority:(size_t)priority
    autoConnect:(BOOL)autoConnect;
{
    yami::agent * a = static_cast<yami::agent *>(agent_);
    
    try
    {
        yami::outgoing_message * omptr;

        if (parameters != nil)
        {
            yami::parameters * params =
                static_cast<yami::parameters *>([parameters getInternals]);
                
            std::auto_ptr<yami::outgoing_message> om(
                a->send([target UTF8String],
                    [objectName UTF8String],
                    [messageName UTF8String],
                    *params, priority, autoConnect));

            omptr = om.release();
        }
        else
        {
            std::auto_ptr<yami::outgoing_message> om(
                a->send([target UTF8String],
                    [objectName UTF8String],
                    [messageName UTF8String],
                    yami::parameters(), priority, autoConnect));

            omptr = om.release();
        }
        
        YAMI4OutgoingMessage * ret = (YAMI4OutgoingMessage *)
            [[YAMI4OutgoingMessage alloc] initWithObject:omptr];

        if (ret != nil)
        {
            return ret;
        }
        else
        {
            delete omptr;
        }
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // normally unreachable
    return nil;
}

-(void)closeConnection:(NSString *)target
{
    [self closeConnection:target priority:0];
}

-(void)closeConnection:(NSString *)target
    priority:(size_t)priority
{
    yami::agent * a = static_cast<yami::agent *>(agent_);

    try
    {
        a->close_connection([target UTF8String], priority);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void *)getInternals
{
    return agent_;
}

@end

@implementation YAMI4IncomingMessage

-(YAMI4IncomingMessage *)initWithObject:(void *)im
{
    self = [super init];
    if (self != nil)
    {
        im_ = im;
    }

    return self;
}

-(NSString *)source
{
    yami::incoming_message * im =
        static_cast<yami::incoming_message *>(im_);

    NSString * ret = [[NSString alloc]
        initWithUTF8String:im->get_source().c_str()];

    return ret;
}

-(NSString *)objectName
{
    yami::incoming_message * im =
        static_cast<yami::incoming_message *>(im_);

    NSString * ret = [[NSString alloc]
        initWithUTF8String:im->get_object_name().c_str()];

    return ret;
}

-(NSString *)messageName
{
    yami::incoming_message * im =
        static_cast<yami::incoming_message *>(im_);

    NSString * ret = [[NSString alloc]
        initWithUTF8String:im->get_message_name().c_str()];

    return ret;
}

-(YAMI4Parameters *)parameters
{
    yami::incoming_message * im =
        static_cast<yami::incoming_message *>(im_);

    yami::parameters * params =
        const_cast<yami::parameters *>(&(im->get_parameters()));
    
    YAMI4Parameters * ret =
        [[YAMI4Parameters alloc] initWithObject:params
            variable:NO owner:NO];

    return ret;
}

-(void)reply
{
    [self reply:nil priority:0];
}

-(void)reply:(YAMI4Parameters *)parameters
{
    [self reply:parameters priority:0];
}

-(void)reply:(YAMI4Parameters *)parameters
    priority:(size_t)priority
{
    yami::incoming_message * im =
        static_cast<yami::incoming_message *>(im_);

    try
    {
        if (parameters != nil)
        {
            yami::parameters * params =
                static_cast<yami::parameters *>([parameters getInternals]);

            im->reply(*params, priority);
        }
        else
        {
            im->reply(yami::parameters(), priority);
        }
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)reject
{
    [self reject:@"" priority:0];
}

-(void)reject:(NSString *)reason
{
    [self reject:reason priority:0];
}

-(void)reject:(NSString *)reason
    priority:(size_t)priority
{
    yami::incoming_message * im =
        static_cast<yami::incoming_message *>(im_);

    try
    {
        im->reject([reason UTF8String], priority);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

@end

// helper, translates message state
YAMI4MessageState translateMessageState(yami::message_state state)
{
    switch (state)
    {
    case yami::posted:
        return YAMI4Posted;
    case yami::transmitted:
        return YAMI4Transmitted;
    case yami::abandoned:
        return YAMI4Abandoned;
    case yami::replied:
        return YAMI4Replied;
    case yami::rejected:
        return YAMI4Rejected;
    default:
        assert(false);
    }
}

@implementation YAMI4OutgoingMessage

-(YAMI4OutgoingMessage *)initWithObject:(void *)om
{
    self = [super init];
    if (self != nil)
    {
        om_ = om;
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
    if (om_ != NULL)
    {
        yami::outgoing_message * om =
            static_cast<yami::outgoing_message *>(om_);
        delete om;
        
        om_ = NULL;
    }
}

-(void)waitForTransmission
{
    yami::outgoing_message * om = static_cast<yami::outgoing_message *>(om_);
    
    try
    {
        om->wait_for_transmission();
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(BOOL)waitForTransmission:(unsigned long long)timeout
{
    yami::outgoing_message * om = static_cast<yami::outgoing_message *>(om_);
    
    try
    {
        return om->wait_for_transmission(timeout);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return NO;
}

-(void)waitForCompletion
{
    yami::outgoing_message * om = static_cast<yami::outgoing_message *>(om_);
    
    try
    {
        om->wait_for_completion();
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(BOOL)waitForCompletion:(unsigned long long)timeout
{
    yami::outgoing_message * om = static_cast<yami::outgoing_message *>(om_);
    
    try
    {
        return om->wait_for_completion(timeout);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return NO;
}

-(enum YAMI4MessageState)state
{
    yami::outgoing_message * om = static_cast<yami::outgoing_message *>(om_);
    
    yami::message_state state = om->get_state();

    return translateMessageState(state);
}

-(enum YAMI4MessageState)state:(size_t *)sentBytes
    totalByteCount:(size_t *)totalByteCount
{
    yami::outgoing_message * om = static_cast<yami::outgoing_message *>(om_);
    
    yami::message_state state = om->get_state(*sentBytes, *totalByteCount);

    return translateMessageState(state);
}

-(YAMI4Parameters *)reply
{
    yami::outgoing_message * om = static_cast<yami::outgoing_message *>(om_);

    try
    {
        yami::parameters * params =
            const_cast<yami::parameters *>(&(om->get_reply()));
    
        YAMI4Parameters * ret =
            [[YAMI4Parameters alloc] initWithObject:params
                variable:NO owner:NO];

        return ret;
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return nil;
}

-(NSString *)exceptionMsg
{
    yami::outgoing_message * om = static_cast<yami::outgoing_message *>(om_);

    NSString * ret = [[NSString alloc]
        initWithUTF8String:om->get_exception_msg().c_str()];

    return ret;
}

@end

