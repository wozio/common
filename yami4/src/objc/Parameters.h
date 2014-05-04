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


@class YAMI4ParameterEntry;
@class YAMI4ParametersIterator;

/// Type of parameter entry.
enum YAMI4ParameterType
{
    YAMI4Boolean,              ///< bool
    YAMI4Integer,              ///< int
    YAMI4LongLong,             ///< long long
    YAMI4Double,               ///< double
    YAMI4String,               ///< string
    YAMI4Binary,               ///< Binary block
    YAMI4BooleanArray,         ///< Array of bool
    YAMI4IntegerArray,         ///< Array of int
    YAMI4LongLongArray,        ///< Array of long long
    YAMI4DoubleArray,          ///< Array of double
    YAMI4StringArray,          ///< Array of strings
    YAMI4BinaryArray,          ///< Array of binary blocks
    YAMI4NestedParameters,     ///< Nested parameters object
    YAMI4NestedParametersArray ///< Nested parameters object
};

/// \brief Collection of message parameters.
///
/// The collection of message parameters, which are typed {name, value} pairs.
///
/// Each entry in this collection has a unique name and can have
/// one of the following types:
/// - bool or bool array
/// - int or int array
/// - long long or long long array
/// - double or double array
/// - string or string array
/// - binary or binary array
/// - nested parameters object, which provides its own scope for naming.
///
/// This class is not thread-safe, although distinct
/// instances of this class can be used by different threads without
/// synchronization.
///
/// <b>Note:</b>
/// The entries are <i>ordered</i> - the order in which they are created
/// influences the final serialized form of the message payload.<br />
/// Newly created entries are appended to the end of the collection unless
/// there is an existing empty slot that can be reused - the appropriate
/// slot is searched for from the beginning to the end of the collection
/// and if no free slot is found the collection is extended at the end.<br />
/// The above guarantee concerns the user code that relies on
/// predictable serialization.
@interface YAMI4Parameters : NSObject
{
    void * params_;
    BOOL variable_;
    BOOL owner_;
}

/// \brief Constructor.
///
/// Creates an empty parameters object.
+(YAMI4Parameters *)new;

/// \brief Initializer.
///
/// Initializes an empty parameters object.
-(YAMI4Parameters *)init;

/// \brief Copying initializer.
///
/// Initializes the parameters object and fills it with copied entries
/// from the given source object.
-(YAMI4Parameters *)initWithParameters:(YAMI4Parameters *) params;

-(YAMI4Parameters *)initWithObject:(void *)p
    variable:(BOOL) variable owner:(BOOL) owner;

/// Finalizer.
///
/// Normally called automatically by the runtime.
-(void)dealloc;

/// \brief Inserts new entry of type bool.
///
/// Inserts a new entry of type bool to the first available slot.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @param value Value to be set.
-(void)setBoolean:(NSString *)name value:(BOOL)value;

/// \brief Extracts the bool value from the given entry.
///
/// Extracts the bool value from the entry given by its name.
/// @param name Name of the entry.
/// @return Value of the entry if it has correct type.
-(BOOL)getBoolean:(NSString *)name;

/// \brief Inserts new entry of type int.
///
/// Inserts a new entry of type int to the first available slot.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @param value Value to be set.
-(void)setInteger:(NSString *)name value:(int)value;

/// \brief Extracts the int value from the given entry.
///
/// Extracts the int value from the entry given by its name.
/// @param name Name of the entry.
/// @return Value of the entry if it has correct type.
-(int)getInteger:(NSString *)name;

/// \brief Inserts new entry of type long long.
///
/// Inserts a new entry of type long long to the first available slot.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @param value Value to be set.
-(void)setLongLong:(NSString *)name value:(long long)value;

/// \brief Extracts the long long value from the given entry.
///
/// Extracts the long long value from the entry given by its name.
/// @param name Name of the entry.
/// @return Value of the entry if it has correct type.
-(long long)getLongLong:(NSString *)name;

/// \brief Inserts new entry of type double.
///
/// Inserts a new entry of type double to the first available slot.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @param value Value to be set.
-(void)setDouble:(NSString *)name value:(double)value;

/// \brief Extracts the double value from the given entry.
///
/// Extracts the double value from the entry given by its name.
/// @param name Name of the entry.
/// @return Value of the entry if it has correct type.
-(double)getDouble:(NSString *)name;

/// \brief Inserts new entry of type string.
///
/// Inserts a new entry of type string to the first available slot.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @param value Value to be set.
-(void)setString:(NSString *)name value:(NSString *)value;

/// \brief Extracts the string value from the given entry.
///
/// Extracts the string value from the entry given by its name.
/// @param name Name of the entry.
/// @return Value of the entry if it has correct type.
///         The return value is newly allocated and should managed by caller.
-(NSString *)getString:(NSString *)name;

/// \brief Inserts new entry of type binary.
///
/// Inserts a new entry of type binary to the first available slot.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// <b>Note:</b> The new entry contains the copy of the given data buffer.
/// @param name Name of the new entry or the entry to be replaced.
/// @param value Value to be set.
/// @param valueLength Value to be set.
-(void)setBinary:(NSString *)name value:(const void *)value
    valueLength:(size_t)valueLength;

/// \brief Extracts the binary value from the given entry.
///
/// Extracts the binary value from the entry given by its name
/// by accessing the buffer directly.
/// @param name Name of the entry.
/// @param valueLength (out) Length of the internal buffer.
/// @return Value of the entry if it has correct type.
///         The returned value points to the internally managed buffer.
-(const void *)getBinary:(NSString *)name valueLength:(size_t *)valueLength;


/// \brief Inserts new entry of type bool array.
///
/// Inserts a new entry of type bool array to the first available slot.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @param values Pointer to the array of values to be set.
/// @param arrayLength Length of the array of values.
///
/// <b>Note:</b> The array of values is <i>copied</i>
/// to the internal buffer.
-(void)setBooleanArray:(NSString *)name values:(BOOL *)values
    arrayLength:(size_t) arrayLength;

/// \brief Extracts the array of bool values from the given entry.
///
/// Extracts the array of bool values from the entry given by its name.
/// @param name Name of the entry.
/// @param arrayLength (out) Length of the internal array.
/// @return Pointer to the newly allocated array - this array should be
///         properly deallocated with free by the caller.
///
-(BOOL *)getBooleanArray:(NSString *)name arrayLength:(size_t *) arrayLength;

/// \brief Inserts new entry of type int array.
///
/// Inserts a new entry of type int array to the first available slot.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @param values Pointer to the array of values to be set.
/// @param arrayLength Length of the array of values.
///
/// <b>Note:</b> The array of values is <i>copied</i>
/// to the internal buffer.
-(void)setIntegerArray:(NSString *)name values:(int *)values
    arrayLength:(size_t) arrayLength;

/// \brief Extracts the array of int values from the given entry.
///
/// Extracts the array of int values from the entry given by its name.
/// @param name Name of the entry.
/// @param arrayLength Length of the internal array.
/// @return Pointer to the internal array buffer.
///
/// <b>Note:</b> this function gives read-write access to
/// the underlying array.
-(int *)getIntegerArray:(NSString *)name arrayLength:(size_t *) arrayLength;

/// \brief Inserts new entry of type long long array.
///
/// Inserts a new entry of type long long array
/// to the first available slot.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @param values Pointer to the array of values to be set.
/// @param arrayLength Length of the array of values.
///
/// <b>Note:</b> The array of values is <i>copied</i>
/// to the internal buffer.
-(void)setLongLongArray:(NSString *)name values:(long long *)values
    arrayLength:(size_t) arrayLength;

/// \brief Extracts the array of long long values from the given entry.
///
/// Extracts the array of long long values
/// from the entry given by its name.
/// @param name Name of the entry.
/// @param arrayLength Length of the internal array.
/// @return Pointer to the internal array buffer.
///
/// <b>Note:</b> this function gives read-write access to
/// the underlying array.
-(long long *)getLongLongArray:(NSString *)name
    arrayLength:(size_t *) arrayLength;

/// \brief Inserts new entry of type double array.
///
/// Inserts a new entry of type double array to the first available slot.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @param values Pointer to the array of values to be set.
/// @param arrayLength Length of the array of values.
///
/// <b>Note:</b> The array of values is <i>copied</i>
/// to the internal buffer.
-(void)setDoubleArray:(NSString *)name values:(double *)values
    arrayLength:(size_t) arrayLength;

/// \brief Extracts the array of double values from the given entry.
///
/// Extracts the array of double values from the entry given by its name.
/// @param name Name of the entry.
/// @param arrayLength Length of the internal array.
///
/// @return Pointer to the internally managed array buffer.
///
/// <b>Note:</b> this function gives read-write access to
/// the underlying array.
-(double *)getDoubleArray:(NSString *)name
    arrayLength:(size_t *) arrayLength;

/// \brief Creates new empty entry of type string array.
///
/// Creates a new empty entry of type string array.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @param arrayLength Length of the newly created array.
///
/// <b>Note:</b> After creation the array contains empty strings
/// (that is, strings which length is 0).
-(void)createStringArray:(NSString *)name arrayLength:(size_t) arrayLength;

/// \brief Inserts new string value to string array.
///
/// Inserts a new string value (possibly replacing the old one)
/// to already existing string array at the given index.
/// @param name Name of the entry containing string array.
/// @param index The array index (array slot, starting from 0).
/// @param value Value to be set.
///
/// <b>Note:</b> The value is <i>copied</i> to the internal buffer.
-(void)setStringInArray:(NSString *)name
    index:(size_t)index value:(NSString *)value;

/// \brief Extracts the length of string array.
///
/// Extracts the length of string array
/// that is located at the given entry.
///
/// @param name Name of the entry containing string array.
/// @return Length of the array.
-(size_t)getStringArrayLength:(NSString *)name;

/// \brief Extracts string value from string array.
///
/// Extracts the string value from the given index of string array.
/// @param name Name of the entry containing string array.
/// @param index The array index (array slot to be read, starting from 0).
/// @return Value from array.
///         The return value is newly allocated and should managed by caller.
-(NSString *)getStringInArray:(NSString *)name index:(size_t)index;

/// \brief Creates new empty entry of type binary array.
///
/// Creates a new empty entry of type binary array.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @param arrayLength Length of the newly created array.
///
/// <b>Note:</b> After creation the array contains empty binaries
/// (that is, binaries which length is 0).
-(void)createBinaryArray:(NSString *)name arrayLength:(size_t) arrayLength;

/// \brief Inserts new binary value to binary array.
///
/// Inserts a new binary value (possibly replacing the old one)
/// to already existing binary array at the given index.
/// @param name Name of the entry containing string array.
/// @param index The array index (array slot, starting from 0).
/// @param value Pointer to the value buffer to be set.
/// @param valueLength Length of the value buffer.
///
/// <b>Note:</b> The value is <i>copied</i> to the internal buffer.
-(void)setBinaryInArray:(NSString *)name
    index:(size_t)index value:(const void *)value
    valueLength:(size_t)valueLength;

/// \brief Extracts the length of binary array.
///
/// Extracts the length of binary array
/// that is located at the given entry.
///
/// @param name Name of the entry containing binary array.
/// @return Length of the array.
-(size_t)getBinaryArrayLength:(NSString *)name;

/// \brief Extracts binary value from binary array.
///
/// Extracts the binary value from the given index of binary array
/// by accessing the internal buffer directly.
/// @param name Name of the entry containing binary array.
/// @param index The array index (array slot to be read, starting from 0).
/// @param valueLength Length of the value buffer.
/// @return Value from array.
-(const void *)getBinaryInArray:(NSString *)name index:(size_t)index
    valueLength:(size_t *)valueLength;

/// \brief Creates nested parameters entry.
///
/// Creates a new nested parameters entry in the first available slot.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @return Pointer to the proxy object that represents the
///         internally created parameters object.
///         The returned value is newly allocated and has to be
///         managed by caller; note that releasing it does not
///         remove the represented entry.
-(YAMI4Parameters *)createNestedParameters:(NSString *)name;

/// \brief Extracts the nested parameters object from the given entry.
///
/// Extracts nested parameters from the entry given by its name.
/// @param name Name of the entry.
/// @return Pointer to the proxy object that represents the internally
///         stored parameters object.
///         The returned value is newly allocated and has to be
///         managed by caller; note that releasing it does not
///         remove the represented entry.
-(YAMI4Parameters *)getNestedParameters:(NSString *)name;

/// \brief Creates new empty entry of type nested parameters array.
///
/// Creates a new empty entry of type parameters array.
/// If the entry with the given name already exists it is replaced
/// without changing the order of entries.
/// @param name Name of the new entry or the entry to be replaced.
/// @param arrayLength Length of the newly created array.
///
/// <b>Note:</b> After creation the array contains empty parameters objects
/// (that is, parameters objects that do not have any entries).
-(void)createNestedArray:(NSString *)name arrayLength:(size_t) arrayLength;

/// \brief Extracts the length of nested parameters array.
///
/// Extracts the length of nested parameters array
/// that is located at the given entry.
///
/// @param name Name of the entry containing nested parameters array.
/// @return Length of the array.
-(size_t)getNestedArrayLength:(NSString *)name;

/// \brief Extracts nested parameters value from nested array.
///
/// Extracts the nested parameters from the given index of nested array.
/// @param name Name of the entry containing nested parameters array.
/// @param index The array index (array slot to be read, starting from 0).
/// @return Value value from array.
-(YAMI4Parameters *)getNestedInArray:(NSString *)name index:(size_t)index;


/// \brief Returns the size of the collection.
///
/// Returns the size of the collection - that is,
/// the number of all non-empty slots.
/// @return the size of the collection
-(size_t)size;

/// \brief Extracts the type of the given entry.
///
/// Extracts the type of the entry given by its name.
/// @param name Name of the entry.
/// @return type of the given entry.
-(enum YAMI4ParameterType)type:(NSString *)name;

/// \brief Extracts the starting iterator this collection.
///
/// Extracts the iterator pointing to the beginning of the collection,
/// which means the first used slot.
/// @return the iterator pointing to the beginning of the collection
-(YAMI4ParametersIterator *)begin;

/// \brief Extracts the ending iterator for this collection.
///
/// Extracts the iterator pointing past the end of the collection.
/// @return the iterator pointing past the end of the collection
-(YAMI4ParametersIterator *)end;

/// \brief Finds the given entry.
///
/// Extracts the view on the entry specified by its name.
/// @param name Name of the entry.
/// @param entry The entry view to be returned. The returned object is
///              newly allocated and has to be managed by caller.
/// @return
///         - <code>YES</code> if the entry has been found
///         - <code>NO</code> if the given name cannot be found
-(BOOL)find:(NSString *)name entry:(YAMI4ParameterEntry * *)entry;

/// \brief Removes the given entry.
///
/// Removes the entry given by its name.
/// @param name Name of the entry to remove.
///
/// <b>Note:</b> The removed entry leaves a <i>hole</i> (empty slot) in
/// the collection that can be reused by newly added entries.
-(void)remove:(NSString *)name;

/// \brief Removes the entry given by its iterator.
///
/// Removes the entry given by its iterator.
/// @param it Iterator pointing to the entry to remove.
///
/// <b>Note:</b> The removed entry leaves a <i>hole</i> (empty slot) in
/// the collection that can be reused by newly added entries.
-(void)removeAt:(YAMI4ParametersIterator *)it;

/// \brief Merges entries from the given parameters object.
///
/// Merges the entries from another parameters object.
/// The merged entries can have the same names,
/// in which case the new entries replace existing ones.
/// The merging is deep in the sense that no data is shared between
/// this and other object after the merge.
/// @param other The object to be merged into this object.
-(void)mergeFrom:(YAMI4Parameters *)other;

/// \brief Clears the collection of entries.
///
/// Clears the collection of entries and deallocates dependent structures.
/// After executing the state of this object is as it was
/// immediately after construction.
-(void)clear;

/// \brief Returns the total size of serialization buffer.
///
/// Computes the total size of serialization buffer(s) for the current
/// content of this object.
-(size_t)serializeBufferSize;

/// \brief Serializes current content into given buffer(s).
///
/// Serializes the current content of this object into the given
/// buffer(s).
/// The serialization buffer does not have to be contiguous and any number
/// of buffer segments is allowed, provided that the size of each buffer
/// segment is a multiple of 4 (32 bits).<br />
/// The function scatters the serialized data into subsequent buffers
/// as they become filled.<br />
/// The buffers are provided as array of buffer pointers and their sizes.
/// @param buffers Pointer to the array of buffer pointers
///        (each of type <code>char *</code>).
/// @param bufferSizes Pointer to the array of buffer sizes.
/// @param numOfBuffers Number of buffers described by the array.
-(void)serialize:(char * *)buffers
     bufferSizes:(size_t *)bufferSizes numOfBuffers:(size_t)numOfBuffers;

/// \brief Deserializes from the given buffer(s).
///
/// Deserializes content from the given buffer(s).
/// The data buffer does not have to be contiguous and any number
/// of buffer segments is allowed, provided that the size of each buffer
/// segment is a multiple of 4 (32 bits).<br />
/// The function gathers the serialized data from subsequent buffers
/// as they are consumed.<br />
/// The buffers are provided as array of buffer pointers and their sizes.
/// @param buffers Pointer to the array of buffer pointers
///        (each of type <code>const char *</code>).
/// @param bufferSizes Pointer to the array of buffer sizes.
/// @param numOfBuffers Number of buffers described by the array.
///
/// <b>Note:</b> The current content of this object is not cleared
/// before attempting deserialization and each retrieved data element
/// is <i>merged</i> into the current content as if done by individual
/// calls to appropriate <code>setXYZ</code> functions.<br />
/// In most cases deserialization will be performed to the empty
/// parameters object (to reconstruct it to the form that was used
/// for serialization), but deserialization onto non-empty object
/// might be occasionally useful as a way of merging two collections.
-(void)deserialize:(const char * *)buffers
     bufferSizes:(size_t *)bufferSizes numOfBuffers:(size_t)numOfBuffers;

-(void *)getInternals;
-(NSString *)dump;

@end


/// \brief Read-only view on the parameters entry.
///
/// Read-only view on the parameters entry.
/// \sa YAMI4Parameters
@interface YAMI4ParameterEntry : NSObject
{
    void * e_;
}

-(YAMI4ParameterEntry *)initWithObject:(void *)e;
-(void)dealloc;

/// \brief Returns the type of underlying (current) entry.
///
/// Returns the type of the underlying entry in the associated
/// parameters object.
/// @return Type of the entry.
-(enum YAMI4ParameterType)type;

/// \brief Extracts the name of current entry.
///
/// Extracts the name of the underlying entry in the associated
/// parameters object.
/// @return Name of the entry.
///         The returned value is newly allocated
///         and has to be managed by caller.
-(NSString *)name;

/// \brief Extracts the bool value from the current entry.
///
/// Extracts the bool value from the current entry.
/// @return Value of the entry if it has correct type.
-(BOOL)getBoolean;

/// \brief Extracts the int value from the current entry.
///
/// Extracts the int value from the current entry.
/// @return Value of the entry if it has correct type.
-(int)getInteger;

/// \brief Extracts the long long value from the current entry.
///
/// Extracts the long long value from the current entry.
/// @return Value of the entry if it has correct type.
-(long long)getLongLong;

/// \brief Extracts the double float value from the current entry.
///
/// Extracts the double float value from the current entry.
/// @return Value of the entry if it has correct type.
-(double)getDouble;

/// \brief Extracts the string value from the current entry.
///
/// Extracts the string value from the current entry.
/// @return Value of the entry if it has correct type.
///         The returned value is newly allocated
///         and has to be managed by caller.
-(NSString *)getString;

/// \brief Extracts the binary value from the current entry.
///
/// Extracts the binary value from the current entry
/// by accessing the buffer directly.
/// @param valueLength Length of the internal buffer.
/// @return Pointer to the internal buffer if it has correct type.
-(const void *)getBinary:(size_t *)valueLength;

/// \brief Extracts the array of bool values from the current entry.
///
/// Extracts the array of bool values from the current entry.
/// @param arrayLength Length of the internal array.
/// @return Pointer to the internal array buffer.
///
/// <b>Note:</b> The return value is a newly allocated buffer
/// that has to be properly freed by the caller.
-(BOOL *)getBooleanArray:(size_t *)arrayLength;

/// \brief Extracts the array of int values from the current entry.
///
/// Extracts the array of int values from the current entry.
/// @param arrayLength Length of the internal array.
/// @return Pointer to the internal array buffer.
///
/// <b>Note:</b> this function gives read-write access to
/// the underlying array.
-(int *)getIntegerArray:(size_t *) arrayLength;

/// \brief Extracts the array of long long values from the current entry.
///
/// Extracts the array of long long values from the current entry.
/// @param arrayLength Length of the internal array.
/// @return Pointer to the internal array buffer.
///
/// <b>Note:</b> this function gives read-write access to
/// the underlying array.
-(long long *)getLongLongArray:(size_t *) arrayLength;

/// \brief Extracts the array of double values from the current entry.
///
/// Extracts the array of double values from the current entry.
/// @param arrayLength Length of the internal array.
/// @return Pointer to the internal array buffer.
///
/// <b>Note:</b> this function gives read-write access to
/// the underlying array.
-(double *)getDoubleArray:(size_t *) arrayLength;

/// \brief Extracts the length of string array.
///
/// Extracts the length of string array
/// that is located at the current entry.
///
/// @return Length of the array.
-(size_t)getStringArrayLength;

/// \brief Extracts string value from string array.
///
/// Extracts the string value from the given index of string array
/// that is located at the current entry.
/// @param index The array index (array slot to be read, starting from 0).
/// @return Value at the given index.
///         The returned value is newly allocated
///         and has to be managed by caller.
-(NSString *)getStringInArray:(size_t)index;

/// \brief Extracts the length of binary array.
///
/// Extracts the length of binary array
/// that is located at the current entry.
///
/// @return Length of the array.
-(size_t)getBinaryArrayLength;

/// \brief Extracts binary value from binary array.
///
/// Extracts the binary value from the given index of binary array
/// that is located at the current entry.
/// @param index The array index (array slot to be read, starting from 0).
/// @param valueLength Length of the internal value buffer.
/// @return Pointer to the internal value buffer.
-(const void *)getBinaryInArray:(size_t)index
    valueLength:(size_t *)valueLength;

/// \brief Extracts the nested parameters value from the current entry.
///
/// Extracts the nested parameters value from the current entry.
/// @return Pointer to the proxy object that represents the internally
///         stored parameters object.
///         The returned value is newly allocated and has to be
///         managed by caller; note that releasing it does not
///         remove the represented entry.
-(YAMI4Parameters *)getNestedParameters;

@end


/// \brief Iterator class for inspecting entries in the collection.
///
/// Iterator class for inspecting entries in the collection.
///
/// This iterator allows to traverse the parameters collection
/// in the forward direction only; it is possible to maintain
/// several independent iterators for the given collection.
///
/// <b>Note:</b> This iterator operates in a way that mimicks the
/// standard C++ iterators, so that it is possible to advance it
/// up to the "end" position (which in fact is a "past the end" position),
/// where dereferencing is no longer possible and the iteration should stop.
///
/// <b>Note:</b> It is not advised to modify the underlying
/// parameters collection by adding or removing entries
/// while the iterator is in use.
@interface YAMI4ParametersIterator : NSObject
{
    void * it_;
}

-(YAMI4ParametersIterator *)initWithValue:(void *)p;
-(void *)getInternals;
-(void)dealloc;

/// \brief Moves the iterator one position forward.
-(void)next;

/// \brief Compares the iterator position to the other iterator.
///
/// Compares the iterator position to the other iterator.
/// <b>Note:</b> The result of this operation is defined only
/// for comparisons with the "end" position.
-(BOOL)isEqual:(YAMI4ParametersIterator *)other;

/// \brief Returns the entry view at the current iterator position.
///
/// @return The entry view at the current iterator position.
///         The returned object is newly allocated
///         and has to be managed by caller.
-(YAMI4ParameterEntry *)entry;

@end
