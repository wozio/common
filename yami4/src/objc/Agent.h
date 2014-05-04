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


@class YAMI4IncomingMessage;
@class YAMI4OutgoingMessage;
@class YAMI4Parameters;
@class YAMI4ValuePublisher;

#define YAMI4TCP_LISTEN_BACKLOG            @"tcp_listen_backlog"
#define YAMI4TCP_REUSEADDR                 @"tcp_reuseaddr"
#define YAMI4TCP_NONBLOCKING               @"tcp_nonblocking"
#define YAMI4TCP_CONNECT_TIMEOUT           @"tcp_connect_timeout"
#define YAMI4TCP_NODELAY                   @"tcp_nodelay"
#define YAMI4TCP_KEEPALIVE                 @"tcp_keepalive"
#define YAMI4TCP_FRAME_SIZE                @"tcp_frame_size"
#define YAMI4UDP_FRAME_SIZE                @"udp_frame_size"
#define YAMI4UNIX_LISTEN_BACKLOG           @"unix_listen_backlog"
#define YAMI4UNIX_NONBLOCKING              @"unix_nonblocking"
#define YAMI4UNIX_FRAME_SIZE               @"unix_frame_size"
#define YAMI4FILE_NONBLOCKING              @"file_nonblocking"
#define YAMI4FILE_FRAME_SIZE               @"file_frame_size"

#define YAMI4DISPATCHER_THREADS            @"dispatcher_threads"
#define YAMI4CONNECTION_RETRIES            @"connection_retries"
#define YAMI4CONNECTION_RETRY_DELAY_SPREAD @"connection_retry_delay_spread"
#define YAMI4OUTGOING_HIGH_WATER_MARK      @"outgoing_high_water_mark"
#define YAMI4OUTGOING_LOW_WATER_MARK       @"outgoing_low_water_mark"
#define YAMI4INCOMING_HIGH_WATER_MARK      @"incoming_high_water_mark"
#define YAMI4INCOMING_LOW_WATER_MARK       @"incoming_low_water_mark"
#define YAMI4DELIVER_AS_RAW_BINARY         @"deliver_as_raw_binary"

#define YAMI4VERSION_NUMBER 10900
#define YAMI4VERSION_NAME  "1.9.0"

/// \brief Incoming message callback protocol.
///
/// This protocol defines a single operation that allows
/// implementers to handle incoming messages.
@protocol YAMI4IncomingMessageCallback

/// \brief Handle incoming message.
///
/// <b>Note:</b> Implementations of this interface should be
/// thread-safe if they are used with the agent that has more than
/// one dispatcher thread.
/// <b>Note:</b> All exceptions thrown from the user code that
/// implements this operation are translated into rejection
/// notifications and sent back to the source site.
///
/// @param message Incoming message.
-(void)call:(YAMI4IncomingMessage *)message;

@end

/// \brief Message broker.
///
/// The message broker that encapsulates physical channel management,
/// incoming and outgoing message queues, listeners and resource
/// management.
///
/// A single agent object can manage many listeners, which are responsible
/// for accepting remote connections, and many incoming and outgoing
/// connections.
///
/// The objects of this class can be safely used by multiple threads.
@interface YAMI4Agent : NSObject
{
    void * agent_;
}

/// \brief Constructor.
///
/// Creates the message broker and starts its internal threads.
/// The broker created with this constructor has no listeners.
+(YAMI4Agent *)new;

/// \brief Initializer.
///
/// Initializes the message broker and starts its internal threads.
/// The broker created with this constructor has no listeners.
-(YAMI4Agent *)init;

/// \brief Initializer.
///
/// Initializes the message broker and starts its internal threads.
/// The broker created with this constructor has no listeners.
///
/// @param options Configuration options for the newly created agent.
-(YAMI4Agent *)initWithOptions:(YAMI4Parameters *)options;

/// \brief Finalizer.
///
/// Calls close. Normally called automatically by the runtime.
-(void)dealloc;

/// \brief Destructor.
///
/// The destructor is called by dealloc,
/// but can be invoked explicitly as well if needed.
/// The destructor stops the internal threads and cleans up all
/// internal data structures.
///
/// <b>Note:</b>
/// The messages and replies that were posted for transmission and that
/// have not yet been fully transmitted are abandoned; in the case of
/// outgoing messages their state is properly notified about that fact.
-(void)close;

/// \brief Adds new listener.
///
/// Adds a new listener for the given target address.
///
/// The supported target formats are:
/// - "tcp://host:port" for TCP/IP connections, where <code>host</code>
///   can be provided in the symbolic or numeric form
/// - "tcp://*:port" for TCP/IP connections, for "any" local address
/// - "tcp://port" for TCP/IP connections, for "any" local address
/// - "udp://host:port" for UDP communication, with rules as for TCP/IP
/// - "unix://path" for Unix connections
///
/// The port for TCP/IP and UDP protocols can be
/// <code>0</code> or <code>*</code>,
/// in which case the actual port number is assigned by the system.
///
/// @param listener The target name for the new listener.
/// @return The locally resolved listener name. This name can be used
///         to remove the listener later on.
///         The returned value is newly allocated
///         and has to be managed by the caller.
-(NSString *)addListener:(NSString *)listener;

/// \brief Removes existing listener.
///
/// Removes the listener denoted by its actual target name.
/// Note that the actual target name might be different from the name
/// provided when the listener was created, due to target resolution.
/// The name which should be used for listener removal is the name
/// that is returned by the <code>addListener</code> function.
-(void)removeListener:(NSString *)listener;

/// \brief Registers the new logical destination object.
///
/// Registers the new "object" that can be a logical destination
/// for incoming messages.
///
/// @param objectName The name of the newly registered object.
///                   If an object with this name is already registered,
///                   the registration data is replaced.
/// @param callback The callable entity that will handle incoming messages.
-(void)registerObject:(NSString *)objectName
    callback:(id <YAMI4IncomingMessageCallback>)callback;

/// \brief Registers the value publisher as a new logical object.
///
/// @param objectName The name of the newly registered object.
///                   If an object with this name is already registered,
///                   the registration data is replaced.
/// @param publisher The value publisher to be registered.
-(void)registerValuePublisher:(NSString *)objectName
    publisher:(YAMI4ValuePublisher *)publisher;

/// \brief Unregisters the logical destination object.
///
/// It is permitted to request unregistration for an object
/// that does not exist - such operation has no effect.
///
/// <b>Note:</b>
/// Due to performance and design tradeoffs it is <b>not</b> guaranteed
/// that no more messages will be ever dispatched to the given object
/// when this function returns.
/// In fact, some of the messages that have been received by agent and not
/// yet dispatched might be still dispatched shortly after
/// this function returns.
/// Only those messages that are received by agent after
/// this function returns are guaranteed not to be dispatched to the
/// unregistered object.
/// This might be particularly important with regard
/// to the lifetime of the callable entity that was provided when
/// the given object has been registered.
///
/// @param objectName The name of the object to be unregistered.
-(void)unregisterObject:(NSString *)objectName;

/// \brief Opens the new connection.
///
/// Opens the new channel or does nothing if the channel already exists.
///
/// This function is not necessary with automatic connection
/// recovery option in <code>send</code> and <code>send_one_way</code>.
///
/// @param target The name of the target endpoint.
///               This name should correspond to the listener name
///               in some target agent object.
-(void)openConnection:(NSString *)target;

/// \brief Opens the new connection with overriding options.
///
/// Opens the new channel or does nothing if the channel already exists.
/// If the new channel is created, it will use the overriding options
/// from those which are defined.
///
/// This function is not necessary with automatic connection
/// recovery option in <code>send</code> and <code>sendOneWay</code>.
///
/// @param target The name of the target endpoint.
///               This name should correspond to the listener name
///               in some target agent object.
/// @param options The set of options that will override agent's values.
-(void)openConnection:(NSString *)target
    options:(YAMI4Parameters *)options;

/// \brief Sends the new outgoing message.
///
/// Sends the new outgoing message to the given destination,
/// with empty payload, default (0) priority and automatic reconnection,
/// and without the possibility to track the message progress.
///
/// See the description and notes for the <code>send</code> function.
-(void)sendOneWay:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName;

/// \brief Sends the new outgoing message.
///
/// Sends the new outgoing message to the given destination,
/// with the given payload, default (0) priority and automatic reconnection,
/// and without the possibility to track the message progress.
///
/// See the description and notes for the <code>send</code> function.
-(void)sendOneWay:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
    parameters:(YAMI4Parameters *)parameters;

/// \brief Sends the new outgoing message.
///
/// Sends the new outgoing message to the given destination,
/// with the given payload, priority and automatic reconnection,
/// and without the possibility to track the message progress.
///
/// See the description and notes for the <code>send</code> function.
-(void)sendOneWay:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
    parameters:(YAMI4Parameters *)parameters
    priority:(size_t)priority;

/// \brief Sends the new outgoing message.
///
/// Sends the new outgoing message to the given destination,
/// with the given payload, priority and automatic reconnection flag,
/// and without the possibility to track the message progress.
///
/// See the description and notes for the <code>send</code> function.
-(void)sendOneWay:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
    parameters:(YAMI4Parameters *)parameters
    priority:(size_t)priority
    autoConnect:(BOOL)autoConnect;

/// \brief Sends the new outgoing message.
///
/// Sends the new outgoing message to the given destination,
/// with empty payload, default (0) priority and automatic reconnection flag.
///
/// See the description and notes for the last <code>send</code> function.
-(YAMI4OutgoingMessage *)send:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName;

/// \brief Sends the new outgoing message.
///
/// Sends the new outgoing message to the given destination,
/// with the given payload, default (0) priority
/// and automatic reconnection flag.
///
/// See the description and notes for the last <code>send</code> function.
-(YAMI4OutgoingMessage *)send:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
    parameters:(YAMI4Parameters *)parameters;

/// \brief Sends the new outgoing message.
///
/// Sends the new outgoing message to the given destination,
/// with the given payload and priority and automatic reconnection flag.
///
/// See the description and notes for the last <code>send</code> function.
-(YAMI4OutgoingMessage *)send:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
    parameters:(YAMI4Parameters *)parameters
    priority:(size_t)priority;

/// \brief Sends the new outgoing message.
///
/// Sends the new outgoing message to the given destination.
///
/// @param target The name of the target endpoint.
///               This name should correspond to the listener name
///               in some target agent object.
/// @param objectName The name of the logical destination object
///                   in the target agent.
/// @param messageName The name of the message.
/// @param parameters The content (payload) of the message.
/// @param priority The priority of the message.
/// @param autoConnect The flag controlling automatic (re)connection.
/// @return The <code>YAMI4OutgoingMessage</code> object that allows to
///         track the progress of this message, its status and obtain
///         response data. The returned object is newly allocated and
///         should be managed by the caller.
///
/// <b>Note:</b>
/// This function implicitly opens a new communication channel
/// if it is not already open. This channel is kept open until
/// it is explicitly closed
/// (see the <code>closeConnection</code> operation)
/// or until the agent is destroyed or the communication error
/// is detected.
-(YAMI4OutgoingMessage *)send:(NSString *)target
    objectName:(NSString *)objectName
    messageName:(NSString *)messageName
    parameters:(YAMI4Parameters *)parameters
    priority:(size_t)priority
    autoConnect:(BOOL)autoConnect;

/// \brief Closes the given communication channel.
///
/// Closes the channel identified by name, with default (0) priority.
///
/// See the documentation of the other <code>closeConnection</code>
/// operation for details.
-(void)closeConnection:(NSString *)target;

/// \brief Closes the given communication channel.
///
/// Closes the channel identified by name.
///
/// The priority allows to properly handle the existing outgoing
/// messages that are waiting in the outgoing queue for transmission.
/// The existing messages with lower priority are
/// abandoned, whereas the existing messages with priority equal
/// or higher to the one provided as parameter are retained in the
/// outgoing queue and are properly pushed for transmission
/// before the channel is physically closed.
/// The channel is closed immediately only if there are no
/// messages waiting in its outgoing queue.
///
/// @param target The name of the target endpoint.
/// @param priority Proprity of the request, respects existing
///        messages in the outgoing queue.
-(void)closeConnection:(NSString *)target
    priority:(size_t)priority;

-(void *)getInternals;

@end

/// \brief Incoming message.
///
/// The proxy allowing to inspect the details of the incoming message
/// and sent back replies or rejection notifications.
///
/// The user code interacts with objects of this type mainly in the
/// callbacl handlers that are provided during object registration and
/// that are later called back when the incoming message arrives.
///
/// <b>Note:</b>
/// The objects of this class are not supposed to be used
/// from multiple threads.
/// They are also created for the purpose of a single callback
/// - if they are not explicitly retained, they are automatically released
/// when the incoming message callback returns.
@interface YAMI4IncomingMessage : NSObject
{
    void * im_;
}

-(YAMI4IncomingMessage *)initWithObject:(void *)im;

/// \brief Returns the source of this incoming message.
///
/// @return The name of the originating endpoint from where this
///         incoming message has been sent.
///         The return value is newly allocated
///         and should be managed by the caller.
-(NSString *)source;

/// \brief Returns the destination object name.
///
/// @return The return value is newly allocated
///         and should be managed by the caller.
-(NSString *)objectName;

/// \brief Returns the message name.
///
/// @return The return value is newly allocated
///         and should be managed by the caller.
-(NSString *)messageName;

/// \brief Provides access to the message content.
///
/// @return The return value is newly allocated
///         and should be managed by the caller.
-(YAMI4Parameters *)parameters;

/// \brief Sends back the reply.
///
/// Sends back the reply with empty payload and default (0) priority.
-(void)reply;

/// \brief Sends back the reply.
///
/// Sends back the reply with the given payload and default (0) priority.
-(void)reply:(YAMI4Parameters *)parameters;

/// \brief Sends back the reply.
///
/// Sends back the reply to the message identified by this object.
/// The reply (or rejection) can be sent only once.
///
/// @param parameters The content of the reply.
/// @param priority The priority of the reply.
-(void)reply:(YAMI4Parameters *)parameters
    priority:(size_t)priority;

/// \brief Sends back the rejection (exception) notification.
///
/// Sends back the rejection to the message identified by this object.
/// The rejection is sent with empty reason description
/// and default (0) priority.
-(void)reject;

/// \brief Sends back the rejection (exception) notification.
///
/// Sends back the rejection to the message identified by this object.
/// The rejection is sent with the given reason description
/// and default (0) priority.
-(void)reject:(NSString *)reason;

/// \brief Sends back the rejection (exception) notification.
///
/// Sends back the rejection to the message identified by this object.
/// The rejection (or reply) can be sent only once.
///
/// @param reason Arbitrary text that will be visible by the message
///               sender as a reason for rejection.
/// @param priority The priority of the rejection.
-(void)reject:(NSString *)reason
    priority:(size_t)priority;

@end

/// Outgoing message state.
enum YAMI4MessageState
{
    YAMI4Posted,      ///< Message was posted for transmission.
    YAMI4Transmitted, ///< Message was fully transmitted.
    YAMI4Abandoned,   ///< Message was abandoned due to error or channel closing.
    YAMI4Replied,     ///< The reply was received for the given message.
    YAMI4Rejected     ///< Message was rejected.
};

/// \brief Outgoing message.
///
/// The proxy allowing to track the progress of outgoing message,
/// inspect its state and to obtain the reply content.
///
/// <b>Note:</b>
/// The objects of this class can be safely used from multiple threads.
@interface YAMI4OutgoingMessage : NSObject
{
    void * om_;
}

-(YAMI4OutgoingMessage *)initWithObject:(void *)om;

/// \brief Finalizer.
///
/// Calls close. Normally called automatically by the runtime.
-(void)dealloc;

/// \brief Destructor.
///
/// Clean up internal resources associated with this message.
/// This operation should be performed if there is no further need
/// to track the message progress.
-(void)close;

/// \brief Waits for the transmission to finish.
///
/// Waits for the transmission to finish - that is, to either send all
/// the message data or to abandon it.
/// After this function returns the state of the message is either
/// <code>YAMI4Transmitted</code>, <code>YAMI4Abandoned</code>,
/// <code>YAMI4Replied</code> or <code>YAMI4Rejected</code>.
-(void)waitForTransmission;

/// \brief Waits for the transmission to finish or until timeout expires.
///
/// Waits for the transmission to finish or until the given relative
/// timeout expires.
///
/// @param timeout The relative timeout in milliseconds.
/// @return
///        - <code>YES</code> if the wait completed before the timeout
///        - <code>NO</code> if the timeout expired before
///          the message was fully transmitted
-(BOOL)waitForTransmission:(unsigned long long)timeout;

/// \brief Waits for the full message roundtrip.
///
/// Waits for the full message roundtrip - that is, for some confirmation
/// that the message has been received and reacted upon by the
/// target agent.
/// After this function returns the state of the message is either
/// <code>YAMI4Abandoned</code>, <code>YAMI4Replied</code> or
/// <code>YAMI4Rejected</code>.
///
/// <b>Note:</b>
/// This function should not be called if the intended semantics of the
/// message is "one-way" - in this case this function would block
/// indefinitely.
-(void)waitForCompletion;

/// \brief Waits for the message roundtrip or until timeout expires.
///
/// Waits for the full message roundtrip or until the given relative
/// timeout expires.
///
/// @param timeout The relative timeout in milliseconds.
/// @return
///        - <code>YES</code> if the wait completed before the timeout
///        - <code>NO</code> if the timeout expired before
///          the message was completed
-(BOOL)waitForCompletion:(unsigned long long)timeout;

/// \brief Returns the state of this message.
-(enum YAMI4MessageState)state;

/// \brief Returns the state of this message.
///
/// This function allows to inspect the progress of the message
/// transmission. During transmission the <code>sentBytes</code> value
/// is always smaller than <code>totalByteCount</code>.
/// When these two values become equal, it means that the transmission
/// was either succesful or abandoned.
///
/// @param sentBytes The number of bytes that were already sent
///                  for this message.
/// @param totalByteCount The total number of bytes that should be sent
///                       for this message.
-(enum YAMI4MessageState)state:(size_t *)sentBytes
    totalByteCount:(size_t *)totalByteCount;

/// \brief Provides access to the reply content.
///
/// The return value is newly allocated and should be managed by the caler.
-(YAMI4Parameters *)reply;

/// \brief Returns the exception message.
///
/// This function can be called when the state of message
/// is <code>YAMI4Rejected</code>.
/// The return value is newly allocated and should be managed by the caler.
-(NSString *)exceptionMsg;

@end

