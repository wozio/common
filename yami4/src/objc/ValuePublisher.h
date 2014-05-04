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

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>


@class YAMI4Agent;
@class YAMI4Parameters;

/// \brief Information about a single subscriber.
///
/// See documentation for the subscribers method.
struct YAMI4SubscriberInfo
{
    NSString * destinationTarget;
    NSString * destinationObject;
};

/// \brief Simple subscription publisher.
///
/// The subscription publisher that notifies remote listeners
/// with published value updates.
///
/// Remote listeners can subscribe and unsubscribe at any time.
@interface YAMI4ValuePublisher : NSObject
{
    void * vp_;
}

/// \brief Constructor.
///
/// Creates the subscription publisher
/// that is not registered at any agent.
+(YAMI4ValuePublisher *)new;

/// \brief Initializer.
///
/// Initializes the subscription publisher
/// that is not registered at any agent.
-(YAMI4ValuePublisher *)init;

/// \brief Finalizer.
///
/// Calls close. Normally called automatically by the runtime.
-(void)dealloc;

/// \brief Destructor.
///
/// The destructor is called by dealloc,
/// but can be invoked explicitly as well if needed.
/// <b>Note:</b>
/// The destructor automatically unregisters the publisher from
/// the agent it was registered at, which might cause hazards
/// if some updates are pending in the incoming message queue.
-(void)close;

/// \brief Registers the publisher at the given agent.
///
/// @param controllingAgent The agent which should manage
///                         the communication for this publisher.
/// @param objectName The name of object that should be visible to
///                   remote subscribers.
-(void)registerAt:(YAMI4Agent *)controllingAgent
    objectName:(NSString *)objectName;

/// \brief Unregisters the publisher from its associated agent.
-(void)unregister;

/// \brief Subscribes the new listener.
///
/// This function is usually called internally as a result of
/// processing the remote "subscribe" message, but can be also
/// used locally if the listener's location is obtained via
/// other means.
///
/// @param destinationTarget Target of the remote listener.
/// @param destinationObject Name of the remote listener's object.
-(void)subscribe:(NSString *)destinationTarget
    destinationObject:(NSString *)destinationObject;

/// \brief Unsubscribes the given listener.
///
/// @param destinationTarget Target of the remote listener.
-(void)unsubscribe:(NSString *)destinationTarget;

/// \brief Publishes the new value.
///
/// Sends the update message to all active listeners
/// with the given value and default (0) priority.
/// See notes for the other publish variant.
-(void)publish:(YAMI4Parameters *)parameters;

/// \brief Publishes the new value.
///
/// Sends the update message to all active listeners
/// with the given value and priority.
/// In case of any errors or communication problems, the problematic
/// listener is automatically unsubscribed.
///
/// @param parameters New value that is to be sent as update to all listeners.
/// @param priority The priority of the update message.
-(void)publish:(YAMI4Parameters *)parameters
    priority:(size_t)priority;

/// \brief Returns the number of active subscribers.
-(size_t)numberOfSubscribers;

/// \brief Returns the information about all active subscribers.
///
/// This function allocates the array of YAMI4SubscriberInfo components
/// - the caller is responsible for deallocating it with free.
/// The first component of each array entry is a destination target
/// and the second component is a destination object for
/// the given subscriber.
-(struct YAMI4SubscriberInfo *)subscribers:(size_t*)numOfSubscribers;

@end

