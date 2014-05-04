# Copyright Maciej Sobczak 2008-2014.
# This file is part of YAMI4.
#
# YAMI4 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# YAMI4 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

import array
import cStringIO
import ctypes
import imp
import os.path
import struct
import sys
import threading

# set to True to use standard extension API instead of ctypes bindings
_use_standard_extension_API = True

# helper functions

def _round_to_4(v):
    """Rounds up for aligning to the 4-byte word boundary."""

    return (v + 3) & ~3

def _append_int(buffer, i):
    """Serializes int value."""

    buffer.write(struct.pack("<i", i))

def _append_long(buffer, i):
    """Serializes long value."""

    buffer.write(struct.pack("<q", i))

def _append_float(buffer, f):
    """Serializes float value."""

    buffer.write(struct.pack("<d", f))

def _append_string_content(buffer, s):
    """Serializes string value (without bounds) to the buffer."""

    # the content should be padded to full 4-byte word
    buffer.write(s)
    length = len(s)
    length_rounded = _round_to_4(length)
    buffer.write((length_rounded - length) * "\x00")

def _append_string(buffer, s):
    """Serializes string value to the buffer."""

    # first place the length of string value
    length = len(s)
    _append_int(buffer, length)

    # the the content
    _append_string_content(buffer, s)

def _append_binary_content(buffer, b):
    """Serializes binary value (without bounds) to the buffer."""

    # the content should be padded to full 4-byte word
    buffer.write(b.value)
    length = len(b.value)
    length_rounded = _round_to_4(length)
    buffer.write((length_rounded - length) * "\x00")

def _append_binary(buffer, b):
    """Serializes string value to the buffer."""

    # first place the length of string value
    length = len(b.value)
    _append_int(buffer, length)

    # the the content
    _append_binary_content(buffer, b)


# public API

def serialize(dictionary):
    """Serializes the content of the given dictionary.

    Returns the binary string with the serialized content."""

    buffer = cStringIO.StringIO()

    #  number of entries
    _append_int(buffer, len(dictionary))

    for e in dictionary.items():
        if e != None:
            name, value = e

            # name of this entry
            _append_string(buffer, name)
                
            # type code and entry value itself
            t = type(value)

            if t == bool:
                _append_int(buffer, 1) # type code for bool
                _append_int(buffer, int(value))

            elif t == int:
                _append_int(buffer, 2) # type code for int
                _append_int(buffer, value)

            elif t == long:
                _append_int(buffer, 3) # type code for long
                _append_long(buffer, value)

            elif t == float:
                _append_int(buffer, 4) # type code for float
                _append_float(buffer, value)

            elif t == str:
                _append_int(buffer, 5) # type code for string
                _append_string(buffer, value)

            elif t == Bytes:
                _append_int(buffer, 6) # type code for bytes
                _append_binary(buffer, value)

            elif t == list:
                t = type(value[0])

                if t == bool:
                    _append_int(buffer, 7) # type code for bool array
                    # pack the array
                    length = len(value)
                    bytes_needed = (length + 7) // 8
                    packed_array = array.array("B", bytes_needed * "\x00")
                    for i in xrange(length):
                        byte_position = i // 8
                        bit_position = i % 8
                        if value[i]:
                            packed_array[byte_position] |= 1 << bit_position
                    _append_int(buffer, length)
                    _append_string_content(buffer, packed_array.tostring())

                elif t == int:
                    _append_int(buffer, 8) # type code for int array
                    _append_int(buffer, len(value))
                    for i in value:
                        _append_int(buffer, i)

                elif t == long:
                    _append_int(buffer, 9) # type code for long array
                    _append_int(buffer, len(value))
                    for i in value:
                        _append_long(buffer, i)

                elif t == float:
                    _append_int(buffer, 10) # float array
                    _append_int(buffer, len(value))
                    for f in value:
                        _append_float(buffer, f)

                elif t == str:
                    _append_int(buffer, 11) # string array
                    _append_int(buffer, len(value))
                    for s in value:
                        _append_string(buffer, s)

                elif t == Bytes:
                    _append_int(buffer, 12) # binary array
                    _append_int(buffer, len(value))
                    for b in value:
                        _append_binary(buffer, b)

                elif t == Parameters:
                    _append_int(buffer, 14) # parameters array
                    _append_int(buffer, len(value))
                    for p in value:
                        buffer.write(p.serialize())

            elif t == Parameters:
                _append_int(buffer, 13) # type code for nested parameters
                buffer.write(value.serialize())

    ret = buffer.getvalue()
    buffer.close()
    return ret


class Parameters(object):
    """Collection of message parameters.
    
    A collection of message parameters is a list of typed {name, value} pairs.
    Each entry in this collection has a unique name and can have
    one of the following types:
        - bool or bool array
        - int or int array
        - long or long array
        - float or float array
        - string or string array
        - binary buffers or their arrays
        - nested Parameters entry, which provides its own scope for naming.
        - nested Parameters arrays

    The names of entries are searched for using
    case-sensitive comparisons.

    Note:
    The instances of this class should not be used from multiple threads
    without synchronization;
    it is safe to use separate instances in separate threads.

    Note:
    The entries are ordered - the order in which they are created
    influences the final serialized form of the message payload.
    Newly created entries are appended to the end of the collection unless
    there is an existing empty slot that can be reused - the appropriate
    slot is searched for from the beginning to the end of the collection
    and if no free slot is found the collection is extended at the end.
    The above guarantee concerns the user code that relies on
    predictable serialization."""

    class __EntryIterator(object):
        """Iterator class for visiting all entries in the collection."""

        ITEMS_MODE = 0
        KEYS_MODE = 1
        VALUES_MODE = 2

        def __init__(self, entries, mode):
            self.__mode = mode
            self.__entries = entries
            self.__next_index = 0
            self.__skip_unused()

        def __iter__(self):
            return self

        def __next__(self):
            return self.next();

        def next(self):
            """Returns the key or item for each used slot."""

            if self.__next_index < len(self.__entries):
                entry = self.__entries[self.__next_index]
                self.__next_index += 1
                self.__skip_unused()
                if self.__mode == self.ITEMS_MODE:
                    return entry
                elif self.__mode == self.KEYS_MODE:
                    return entry[0]
                else: # VALUES_MODE
                    return entry[1]
            else:
                raise StopIteration

        def __skip_unused(self):
            found = False
            for i in range(self.__next_index, len(self.__entries)):
                if self.__entries[i] != None:
                    self.__next_index = i
                    found = True
                    break

            if not found:
                self.__next_index = len(self.__entries)

    def __init__(self):
        """Default constructor, creates an empty collection of parameters."""

        self.__entries = []
        self.__entry_indexes = dict()   # name -> index in entries

    def __validate_value(self, value):
        """Checks whether the given value is of the accepted type.

        For iterables, in addition to validating that all elements have
        the same type, makes and returns a deep copy of the sequence."""

        basic_types = [bool, int, long, float, str, Bytes]

        t = type(value)
        if t in basic_types:
            return value
        elif t == Parameters:
            # parameters nesting
            return value
        else:
            # none of the accepted basic types - try iteration
            try:
                it = iter(value)
                v = it.next()
            except StopIteration:
                # iteration empty (failed when accessing first element)
                raise TypeError("Empty sequences are not supported.")
            except Exception, e:
                # iteration failed (not an iterator?)
                raise TypeError(
                    "The type \"" + str(t) + "\' is not supported.")

            t = type(v)
            if t not in basic_types and t != Parameters:
                raise TypeError(
                    "The type \"" + str(t) + "\' is not supported.")

            deep_copy = [v]
            for v in it:
                if type(v) != t:
                    raise TypeError("Sequences must be homogenous.")
                deep_copy.append(v)

            return deep_copy
            
    def __setitem__(self, name, value):
        """Sets the given value in the named slot.

        If the given slot is already used, its value is replaced."""

        value = self.__validate_value(value)

        if name in self.__entry_indexes:
            # replace existing entry
            index = self.__entry_indexes[name]
            self.__entries[index] = (name, value)

        else:
            # create new entry
            if None in self.__entries:
                # reuse existing empty slot
                index = self.__entries.index(None)
                self.__entries[index] = (name, value)

            else:
                # extend the list for new element
                index = len(self.__entries)
                self.__entries.append((name, value))

            self.__entry_indexes[name] = index

    def __getitem__(self, name):
        """Returns the value of the named slot."""

        if name in self.__entry_indexes:
            return self.__entries[self.__entry_indexes[name]][1]
        else:
            raise KeyError(
                "Entry or object named \'" + name + "\' does not exist.")

    def __delitem__(self, name):
        """Removes the given entry from the collection.

        Note: The removed entry leaves a hole (empty slot) in
        the collection that can be reused by newly added entries."""

        if name in self.__entry_indexes:
            self.__entries[self.__entry_indexes[name]] = None
            del self.__entry_indexes[name]
        else:
            raise KeyError(
                "Entry or object named \'" + name + "\' does not exist.")

    def __len__(self):
        """Gets the current size of the collection."""

        return len(self.__entry_indexes)

    def __in__(self, name):
        """Checks if the entry with the given name (key) exists."""

        return name in self.__entry_indexes

    def __iter__(self):
        """Gets the iterator over the list of keys.

        The iterator visits only those entries which are used
        (in other words, it skips unused slots)."""

        return Parameters.__EntryIterator(
            self.__entries, Parameters.__EntryIterator.KEYS_MODE)

    def keys(self):
        """Gets the iterator over the list of keys.

        The iterator visits only those entries which are used
        (in other words, it skips unused slots)."""

        return self.__iter__()

    def values(self):
        """Gets the iterator over the list of values.

        The iterator visits only those entries which are used
        (in other words, it skips unused slots)."""

        return Parameters.__EntryIterator(
            self.__entries, Parameters.__EntryIterator.VALUES_MODE)

    def items(self):
        """Gets the iterator over the list of items.

        The iterator visits only those entries which are used
        (in other words, it skips unused slots)."""

        return Parameters.__EntryIterator(
            self.__entries, Parameters.__EntryIterator.ITEMS_MODE)

    def serialize(self):
        """Serializes the content of the whole collection.

        Returns the binary string with the serialized content."""

        return serialize(self)

    def __extract_int(self, buf, offset):
        """Deserializes int from the binary buffer.

        Returns tuple (value, newoffset),
        where newoffset points just after the element that was read."""

        return struct.unpack_from("<i", buf, offset)[0], offset + 4

    def __extract_long(self, buf, offset):
        """Deserializes long from the binary buffer.

        Returns tuple (value, newoffset),
        where newoffset points just after the element that was read."""

        return long(struct.unpack_from("<q", buf, offset)[0]), offset + 8

    def __extract_float(self, buf, offset):
        """Deserializes float from the binary buffer.

        Returns tuple (value, newoffset),
        where newoffset points just after the element that was read."""

        return struct.unpack_from("<d", buf, offset)[0], offset + 8

    def __extract_string_content(self, buf, offset, length):
        """Deserializes string (with given bounds) from the binary buffer.

        Returns tuple (value, newoffset),
        where newoffset points after the string that was read,
        rounded up to 4."""
        
        value = str(buf[offset:(offset + length)])
        offset = _round_to_4(offset + length)
        return value, offset

    def __extract_string(self, buf, offset):
        """Deserializes string from the binary buffer.

        Returns tuple (value, newoffset),
        where newoffset points after the string that was read,
        rounded up to 4."""

        length, offset = self.__extract_int(buf, offset)
        value, offset = self.__extract_string_content(buf, offset, length)
        return value, offset
        
    def __extract_binary_content(self, buf, offset, length):
        """Deserializes binary (with given bounds) from the binary buffer.

        Returns tuple (value, newoffset),
        where newoffset points after the binary that was read,
        rounded up to 4."""
        
        value = buf[offset:(offset + length)]
        offset = _round_to_4(offset + length)
        return Bytes(value), offset

    def __extract_binary(self, buf, offset):
        """Deserializes binary from the binary buffer.

        Returns tuple (value, newoffset),
        where newoffset points after the binary that was read,
        rounded up to 4."""

        length, offset = self.__extract_int(buf, offset)
        value, offset = self.__extract_binary_content(buf, offset, length)
        return value, offset
        
    def deserialize(self, buf):
        """Deserializes from the given buffer.

        Note: The current content of this object is not cleared
        before attempting deserialization and each retrieved data element
        is merged into the current content as if done by inserting entries
        individually.
        In most cases deserialization will be performed to the empty
        Parameters object (to reconstruct it to the form that was used
        for serialization), but deserialization onto non-empty object
        might be occasionally useful as a way of merging two collections."""

        offset = 0
        offset = self.__do_deserialize(buf, offset)

        if offset !=  len(buf):
            # not all content was deserialized - corrupted buffer
            raise ValueError("Unexpected value in the buffer.")

    def __do_deserialize(self, buf, offset):
        """Recursive helper for deserialize."""

        num_of_entries, offset = self.__extract_int(buf, offset)

        for i in range(num_of_entries):
            name, offset = self.__extract_string(buf, offset)
            type_code, offset = self.__extract_int(buf, offset)

            if type_code == 1: # bool
                value, offset = self.__extract_int(buf, offset)
                self.__setitem__(name, bool(value))

            elif type_code == 2: # int
                value, offset = self.__extract_int(buf, offset)
                self.__setitem__(name, value)

            elif type_code == 3: # long
                value, offset = self.__extract_long(buf, offset)
                self.__setitem__(name, value)

            elif type_code == 4: # float
                value, offset = self.__extract_float(buf, offset)
                self.__setitem__(name, value)

            elif type_code == 5: # string
                value, offset = self.__extract_string(buf, offset)
                self.__setitem__(name, value)

            elif type_code == 6: # binary
                value, offset = self.__extract_binary(buf, offset)
                self.__setitem__(name, value)

            elif type_code == 7: # bool array
                length, offset = self.__extract_int(buf, offset)
                bytes_needed = (length + 7) // 8
                packed_array, offset = self.__extract_binary_content(
                    buf, offset, bytes_needed)
                value = []
                for j in range(length):
                    byte_position = j // 8
                    bit_position = j % 8
                    value.append((ord(packed_array.value[byte_position]) &
                                  (1 << bit_position)) != 0)
                self.__setitem__(name, value)

            elif type_code == 8: # int array
                length, offset = self.__extract_int(buf, offset)
                value = []
                for j in range(length):
                    v, offset = self.__extract_int(buf, offset)
                    value.append(v)
                self.__setitem__(name, value)

            elif type_code == 9: # long array
                length, offset = self.__extract_int(buf, offset)
                value = []
                for j in range(length):
                    v, offset = self.__extract_long(buf, offset)
                    value.append(v)
                self.__setitem__(name, value)

            elif type_code == 10: # float array
                length, offset = self.__extract_int(buf, offset)
                value = []
                for j in range(length):
                    v, offset = self.__extract_float(buf, offset)
                    value.append(v)
                self.__setitem__(name, value)

            elif type_code == 11: # string array
                length, offset = self.__extract_int(buf, offset)
                value = []
                for j in range(length):
                    v, offset = self.__extract_string(buf, offset)
                    value.append(v)
                self.__setitem__(name, value)

            elif type_code == 12: # binary array
                length, offset = self.__extract_int(buf, offset)
                value = []
                for j in range(length):
                    v, offset = self.__extract_binary(buf, offset)
                    value.append(v)
                self.__setitem__(name, value)

            elif type_code == 13: # nested parameters
                nested = Parameters()
                offset = nested.__do_deserialize(buf, offset)
                self.__setitem__(name, nested)

            elif type_code == 14: # nested array
                length, offset = self.__extract_int(buf, offset)
                value = []
                for j in range(length):
                    n = Parameters()
                    offset = n.__do_deserialize(buf, offset)
                    value.append(n)
                self.__setitem__(name, value)

            else:
                raise ValueError("Unexpected value in the buffer.")

        return offset

    def __str__(self):
        """Returns a string representation of the content.

        This function is supposed to be used by unit tests and for
        simple debugging."""

        str = cStringIO.StringIO()
        self.__build_string(str, "")
        ret = str.getvalue()
        str.close()
        return ret

    def __build_string(self, buf, prefix):
        """Recursive helper for __str__."""

        for i in range(len(self.__entries)):
            buf.write(prefix + "entry " + str(i) + ":\n")
            add_newline = True

            e = self.__entries[i]
            if e == None:
                buf.write(prefix + "unused")
            else:
                name, value = e
                buf.write(prefix + "name: " + name + "\n")

                t = type(value)

                if t == bool:
                    buf.write(prefix + "bool: " + str(int(value)))

                elif t == int:
                    buf.write(prefix + "int: " + str(value))

                elif t == long:
                    buf.write(prefix + "long: " + str(value))

                elif t == float:
                    buf.write(prefix + "float: " + str(value))

                elif t == str:
                    buf.write(prefix + "string: " + value)

                elif t == Bytes:
                    buf.write(prefix + "binary of length " +
                              str(len(value.value)))

                elif t == list:
                    t = type(value[0])

                    if t == bool:
                        buf.write(prefix + "bool array:")
                        for v in value:
                            buf.write(" " + str(int(v)))

                    elif t == int:
                        buf.write(prefix + "int array:")
                        for v in value:
                            buf.write(" " + str(v))

                    elif t == long:
                        buf.write(prefix + "long array:")
                        for v in value:
                            buf.write(" " + str(v))

                    elif t == float:
                        buf.write(prefix + "float array:")
                        for v in value:
                            buf.write(" " + str(v))

                    elif t == str:
                        buf.write(prefix + "string array:")
                        for v in value:
                            buf.write(" " + v)

                    elif t == Bytes:
                        buf.write(prefix + "binary array of length " +
                                  str(len(value)))

                    elif t == Parameters:
                        buf.write(prefix + "nested parameters array of length " +
                                  str(len(value)) + ":\n")
                        i = 0
                        for v in value:
                            buf.write(prefix + "  nested at index " + str(i) + ":\n")
                            i = i + 1
                            v.__build_string(buf, prefix + "    ")
                        add_newline = False

                elif t == Parameters:
                    buf.write(prefix + "nested parameters:\n")
                    value.__build_string(buf, prefix + "  ")
                    add_newline = False

            if add_newline:
                buf.write("\n")


class Bytes(object):
    """Typed wrapper for binary arrays.

    It is possible to directly access the 'value' attribute of this object
    it is equal to the string used to initialize this object."""

    def __init__(self, content):
        self.value = content

    def __str__(self):
        return self.value

    def __hash__(self):
        return self.value.__hash__()


class YAMIError(Exception):
    """Exception type for YAMI-related run-time and logical errors."""
    pass

def _pointer(result):
    """Extracts the pointer result from underlying library."""

    if result == None:
        raise YAMIError("Not enough memory to allocate result object.")

    if yami4py.yami4_is_success(result):
        p = yami4py.yami4_get_pointer(result)
        yami4py.yami4_destroy_result(result)
        if _use_standard_extension_API:
            # in case of standard extension API the pointer value
            # is encoded as unsigned long long
            # and does not need any further processing
            return p
        else:
            return ctypes.c_void_p(p)
    else:
        error = yami4py.yami4_get_error(result)
        yami4py.yami4_destroy_result(result)
        raise YAMIError(str(error))

def _string(result):
    """Extracts the string result from underlying library."""

    if result == None:
        raise YAMIError("Not enough memory to allocate result object.")

    if yami4py.yami4_is_success(result):
        s = yami4py.yami4_get_string(result)
        yami4py.yami4_destroy_result(result)
        return str(s)
    else:
        error = yami4py.yami4_get_error(result)
        yami4py.yami4_destroy_result(result)
        raise YAMIError(str(error))

def _binary(result):
    """Extracts the binary result from underlying library."""

    if result == None:
        raise YAMIError("Not enough memory to allocate result object.")

    if yami4py.yami4_is_success(result):
        p = yami4py.yami4_get_pointer(result)
        size = yami4py.yami4_get_int_i(result)
        yami4py.yami4_destroy_result(result)
        # TODO: faster method? a custom buffer class, perhaps?
        bin = array.array("B", size * "\x00")

        if _use_standard_extension_API:
            for i in range(size):
                # note: bytes (c_bytes) from ctypes are signed,
                # whereas bytearray expects unsigned ranges
                v = yami4py.yami4_read_from_binary_array(p, i)
                if v >= 0:
                    bin[i] = v
                else:
                    bin[i] = 256 + v
        else:
            p = ctypes.cast(p, ctypes.POINTER(ctypes.c_byte))
            for i in range(size):
                # note: bytes (c_bytes) from ctypes are signed,
                # whereas bytearray expects unsigned ranges
                v = p[i]
                if v >= 0:
                    bin[i] = v
                else:
                    bin[i] = 256 + v
        return bin.tostring()
    else:
        error = yami4py.yami4_get_error(result)
        yami4py.yami4_destroy_result(result)
        raise YAMIError(str(error))

def _check(result):
    """Checks result for error condition."""

    if result == None:
        raise YAMIError("Not enough memory to allocate result object.")

    if yami4py.yami4_is_success(result) == 0:
        error = yami4py.yami4_get_error(result)
        yami4py.yami4_destroy_result(result)
        raise YAMIError(str(error))
    yami4py.yami4_destroy_result(result)
        
def _utf8(s):
    """Converts the given string to sequence of bytes according to UTF-8."""
    return s.encode("utf8")

# API bound to dynamic library:

class OutgoingMessage(object):
    """Outgoing message.

    The handler allowing to track the progress of outgoing message,
    inspect its state and to obtain the reply content.

    Note:
    The objects of this class can be safely used from multiple threads."""

    POSTED = 1
    TRANSMITTED = 2
    ABANDONED = 3
    REPLIED = 4
    REJECTED = 5

    def __init__(self, msg):
        self.__msg = msg

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()

    def __del__(self):
        if self.__msg != None:
            self.close()
            
    def close(self):
        """Deallocates internal resources associated with this object.

        This function is called automatically if the object is used
        as a context manager."""

        yami4py.yami4_destroy_outgoing_message(self.__msg)
        self.__msg = None

    def get_state(self):
        """Returns the state of this message.

        This function allows to inspect the progress of the message
        transmission and returns a 3-tuple: state, sent and total_byte_count.
        During transmission the sent value
        is always smaller than total_byte_count.
        When these two values become equal, it means that the transmission
        was either succesful or abandoned."""

        result = yami4py.yami4_outgoing_message_get_state(self.__msg)
        state = yami4py.yami4_get_int_i(result)
        bytes_sent = yami4py.yami4_get_int_j(result)
        total_byte_count = yami4py.yami4_get_int_k(result)
        yami4py.yami4_destroy_result(result)
        return state, bytes_sent, total_byte_count

    def wait_for_transmission(self, timeout = 0):
        """Waits for the transmission to finish.

        Waits for the transmission to finish - that is, to either send all
        the message data or to abandon it.
        If the timeout value is greater than 0, it means relative timeout
        in milliseconds; the function returns True if the transmission was
        finished before the timeout expired and False otherwise.
        If the timeout value is non-positive, there is no timeout and the
        function can wait indefinitely.
        After this function returns True the state of the message is either
        TRANSMITTED, ABANDONED, REPLIED or REJECTED."""

        _check(yami4py.yami4_outgoing_message_wait_for_transmission(
            self.__msg, timeout))

    def wait_for_completion(self, timeout = 0):
        """Waits for the full message roundtrip.
    
        Waits for the full message roundtrip - that is, for some confirmation
        that the message has been received and reacted upon by the
        target agent.
        If the timeout value is greater than 0, it means relative timeout
        in milliseconds; the function returns True if the message was
        completed before the timeout expired and False otherwise.
        If the timeout value is non-positive, there is no timeout and the
        function can wait indefinitely.
        After this function returns True the state of the message is either
        ABANDONED, REPLIED or REJECTED.
    
        Note:
        This function should not be called if the intended semantics of the
        message is "one-way" - in this case this function would block
        indefinitely."""

        _check(yami4py.yami4_outgoing_message_wait_for_completion(
                self.__msg, timeout))

    def get_reply(self):
        """Provides access to the reply content."""

        params = Parameters()
        params.deserialize(_binary(
                yami4py.yami4_outgoing_message_get_raw_reply(self.__msg)))
        return params

    def get_exception_msg(self):
        """Returns the human-readable reason for message rejection."""

        return _string(
            yami4py.yami4_outgoing_message_get_exception_msg(self.__msg))


class IncomingMessage(object):
    """Incoming message.

    The handler allowing to inspect the details of the incoming message
    and sent back replies or rejection notifications.

    The user code interacts with objects of this type mainly in the
    functors that are provided during object registration and that are later
    called back when the incoming message arrives. The handler objects
    can be stored aside for further processing even after the callback
    returns, but should not be kept alive longer than the agent itself.

    Note:
    The objects of this class are not supposed to be used
    from multiple threads."""

    def __init__(self, msg):
        self.__msg = msg

    def __del__(self):
        if self.__msg != None:
            self.close()
            
    def close(self):
        """Deallocates internal resources associated with this object.

        This function is called automatically if the object is used
        as a context manager."""

        yami4py.yami4_destroy_incoming_message(self.__msg)
        self.__msg = None

    def get_source(self):
        """Returns the source of this incoming message."""

        return _string(
            yami4py.yami4_incoming_message_get_source(self.__msg))

    def get_object_name(self):
        """Returns the destination object name."""

        return _string(
            yami4py.yami4_incoming_message_get_object_name(self.__msg))

    def get_message_name(self):
        """Returns the message name."""

        return _string(
            yami4py.yami4_incoming_message_get_message_name(self.__msg))

    def get_parameters(self):
        """Provides access to the message content."""

        params = Parameters()
        params.deserialize(_binary(
                yami4py.yami4_incoming_message_get_raw_content(self.__msg)))
        return params

    def reply(self, content = {}, priority = 0):
        """Sends back the reply.
        
        Sends back the reply to the message identified by this object.
        The reply (or rejection) can be sent only once."""

        serialized_content = serialize(content)
        yami4py.yami4_incoming_message_reply(
            self.__msg, serialized_content, len(serialized_content),
            priority)

    def reject(self, reason = "", priority = 0):
        """Sends back the rejection (exception) notification.
        
        Sends back the rejection to the message identified by this object.
        The rejection (or reply) can be sent only once."""

        yami4py.yami4_incoming_message_reject(
            self.__msg, _utf8(reason), priority)


class Agent(object):
    """Message broker.
    
    The message broker that encapsulates physical channel management,
    incoming and outgoing message queues, listeners and resource
    management.

    A single agent object can manage many listeners, which are responsible
    for accepting remote connections, and many incoming and outgoing
    connections.

    The agent objects can be created and destroyed without constraints
    on the stack, on the free store or as static objects.

    The objects of this class can be safely used by multiple threads."""

    # connection event values
    NEW_INCOMING_CONNECTION = 1
    NEW_OUTGOING_CONNECTION = 2
    CONNECTION_CLOSED = 3
    
    class OptionNames(object):

        # core option names
        TCP_LISTEN_BACKLOG =  "tcp_listen_backlog"
        TCP_REUSEADDR =       "tcp_reuseaddr"
        TCP_NONBLOCKING =     "tcp_nonblocking"
        TCP_CONNECT_TIMEOUT = "tcp_connect_timeout"
        TCP_NODELAY =         "tcp_nodelay"
        TCP_KEEPALIVE =       "tcp_keepalive"
        TCP_FRAME_SIZE =      "tcp_frame_size"
        UDP_FRAME_SIZE =      "udp_frame_size"
        UNIX_LISTEN_BACKLOG = "unix_listen_backlog"
        UNIX_NONBLOCKING =    "unix_nonblocking"
        UNIX_FRAME_SIZE =     "unix_frame_size"
        FILE_NONBLOCKING =    "file_nonblocking"
        FILE_FRAME_SIZE =     "file_frame_size"

        # C++ general-purpose option names
        DISPATCHER_THREADS =            "dispatcher_threads"
        CONNECTION_RETRIES =            "connection_retries"
        CONNECTION_RETRY_DELAY_SPREAD = "connection_retry_delay_spread"
        OUTGOING_HIGH_WATER_MARK =      "outgoing_high_water_mark"
        OUTGOING_LOW_WATER_MARK =       "outgoing_low_water_mark"
        INCOMING_HIGH_WATER_MARK =      "incoming_high_water_mark"
        INCOMING_LOW_WATER_MARK =       "incoming_low_water_mark"

        # note: this is not available in Python
        # and hardcoded for the underlying C++ component
        #DELIVER_AS_RAW_BINARY =         "deliver_as_raw_binary"

        # additional Python settings
        INCOMING_QUEUE_MAX_LENGTH = "incoming_queue_max_length"

    class __DispatcherThread(threading.Thread):
        """Dispatcher thread that consumes incoming messages from the queue
        and delivers them to registered callable entities."""

        def __init__(self, agent, object_map, object_map_lock,
                     connection_event_callback):
            self.__agent = agent
            self.__objects = object_map
            self.__objects_lock = object_map_lock
            self.__connection_event_callback = connection_event_callback
            threading.Thread.__init__(self)

        def run(self):
            while True:

                # first check if there is a regular incoming message
                msg_ptr = yami4py.yami4_agent_get_next_incoming_message(
                    self.__agent)
                if msg_ptr == None:
                    # there is no incoming message -> check connection events
                    conn_event = _string(
                        yami4py.yami4_agent_get_next_connection_event(
                            self.__agent))
                    if conn_event:
                        if self.__connection_event_callback:
                            if conn_event[0] == 'i':
                                event = Agent.NEW_INCOMING_CONNECTION
                            elif conn_event[0] == 'o':
                                event = Agent.NEW_OUTGOING_CONNECTION
                            else:
                                event = Agent.CONNECTION_CLOSED

                            connection_name = conn_event[2:]
                            try:
                                self.__connection_event_callback(
                                    connection_name, event)
                            except:
                                # ignore exceptions from user code
                                pass

                        # continue checking the queue
                        continue

                    else:
                        # no incoming message and no connection event
                        # -> agent closing
                        return

                else:
                    # process incoming message

                    msg = IncomingMessage(_pointer(msg_ptr))

                    object_name = msg.get_object_name()

                    handler = None
                    self.__objects_lock.acquire()
                    try:
                        try:
                            handler = self.__objects[object_name]
                        except KeyError:
                            # no such object -> try the default handler
                            if "*" in self.__objects:
                                handler = self.__objects["*"]
                
                        if handler != None:
                            # object handler found, call it
                            try:
                                handler(msg)
                            except Exception, e:
                                msg.reject(str(e))                        
                        else:
                            # reject the message
                            msg.reject("Unknown destination object.")
                        
                        msg.close()
                    finally:
                        self.__objects_lock.release()


    def __init__(self, options = {}, connection_event_callback = None):
        self.__objects = {}
        self.__default_object = None
        self.__objects_lock = threading.Lock()
        if Agent.OptionNames.DISPATCHER_THREADS in options:
            self.__dispatcher_threads = \
                options[Agent.OptionNames.DISPATCHER_THREADS]
            del options[Agent.OptionNames.DISPATCHER_THREADS]
        else:
            self.__dispatcher_threads = 1
        serialized_options = serialize(options)
        self.__agent = _pointer(yami4py.yami4_create_agent(
            serialized_options, len(serialized_options)))
        
        # start the dispatcher thread(s)

        self.__dispatchers = []
        for i in range(self.__dispatcher_threads):
            dispatcher = Agent.__DispatcherThread(
                self.__agent, self.__objects, self.__objects_lock,
                connection_event_callback)
            dispatcher.daemon = True
            dispatcher.start()
            self.__dispatchers.append(dispatcher)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()

    def __del__(self):
        if self.__agent != None:
            self.close()
            
    def close(self):
        """Deallocates internal resources associated with this object.

        This function is called automatically if the object is used
        as a context manager."""

        # stop the dispatcher thread(s)
        for i in range(self.__dispatcher_threads):
            yami4py.yami4_agent_terminate_incoming_queue(self.__agent)

        for i in range(self.__dispatcher_threads):
            self.__dispatchers[i].join()
        
        yami4py.yami4_destroy_agent(self.__agent)
        self.__agent = None

    def add_listener(self, listener):
        """Adds new listener.
        
        Adds a new listener for the given target address.
        
        The supported target formats are:
            - "tcp://host:port" for TCP/IP connections, where host
            can be provided in the symbolic or numeric form
            - "tcp://*:port" for TCP/IP connections, for "any" local address
            - "tcp://port" for TCP/IP connections, for "any" local address
            - "udp://host:port" for UDP communication
            - "unix://path" for Unix connections
        
        The port for TCP/IP and UDP protocols can be 0 or *,
        in which case the actual port number is assigned by the system."""

        return _string(yami4py.yami4_agent_add_listener(
                self.__agent, _utf8(listener)))

    def remove_listener(self, listener):
        """Removes existing listener.
        
        Removes the listener denoted by its actual target name.
        Note that the actual target name might be different from the name
        provided when the listener was created, due to target resolution.
        The name which should be used for listener removal is the name
        that is returned by the add_listener function."""

        yami4py.yami4_agent_remove_listener(
                self.__agent, _utf8(listener))

    def register_object(self, object_name, callback):
        """Registers the new logical destination object.
        
        Registers the new "object" that can be a logical destination
        for incoming messages."""

        self.__objects_lock.acquire()
        try:
            self.__objects[object_name] = callback
        finally:
            self.__objects_lock.release()

    def register_value_publisher(self, object_name, publisher):
        """Registers the value publisher as a new logical object."""

        publisher.register_at(self, object_name)

    def unregister_object(self, object_name):
        """Unregisters the logical destination object.
        
        It is permitted to request unregistration for an object
        that does not exist - such operation has no effect.
        
        Note:
        Due to performance and design tradeoffs it is not guaranteed
        that no more messages will be ever dispatched to the given object
        when this function returns.
        In fact, some of the messages that have been received by agent and not
        yet dispatched might be still dispatched shortly after
        this function returns.
        Only those messages that are received by agent after
        this function returns are guaranteed not to be dispatched to the
        unregistered object.
        This might be particularly important with regard
        to the lifetime of the callable entity that was provided when
        the given object has been registered."""

        self.__objects_lock.acquire()
        try:
            try:
                del self.__objects[object_name]
            except KeyError:
                pass
        finally:
            self.__objects_lock.release()
        
    def open_connection(self, target):
        """Opens the new connection.
        
        Opens the new channel or does nothing if the channel already exists.

        This function is not necessary with automatic connection
        recovery option in send and send_one_way."""

        _check(yami4py.yami4_agent_open_connection(
                self.__agent, _utf8(target)))

    def send(self, target, object_name, message_name,
                     content = {}, priority = 0, auto_connect = True):
        """Sends the new outgoing message.
        
        Sends the new outgoing message to the given destination.
        
        Note:
        This function implicitly opens a new communication channel
        if it is not already open. This channel is kept open until
        it is explicitly closed
        (see the close_connection function)
        or until the agent is destroyed or the communication error
        is detected."""

        serialized_content = serialize(content)
        msg = OutgoingMessage(
            _pointer(yami4py.yami4_agent_send(self.__agent,
                    _utf8(target), _utf8(object_name), _utf8(message_name),
                    serialized_content, len(serialized_content), priority,
                    1 if auto_connect else 0)))
        return msg

    def send_one_way(self, target, object_name, message_name,
                     content = {}, priority = 0, auto_connect = True):
        """Sends the new outgoing message.
        
        Sends the new outgoing message to the given destination, without
        the possibility to track its progress.
        
        See the description and notes for the send function."""

        serialized_content = serialize(content)
        _check(yami4py.yami4_agent_send_one_way(self.__agent,
                _utf8(target), _utf8(object_name), _utf8(message_name),
                serialized_content, len(serialized_content), priority,
                1 if auto_connect else 0))

    def close_connection(self, target, priority = 0):
        """Closes the given communication channel.
        
        Closes the channel identified by name.
        
        The priority allows to properly handle the existing outgoing
        messages that are waiting in the outgoing queue for transmission.
        The existing messages with lower priority are
        abandoned, whereas the existing messages with priority equal
        or higher to the one provided as parameter are retained in the
        outgoing queue and are properly pushed for transmission
        before the channel is physically closed.
        The channel is closed immediately only if there are no
        messages waiting in its outgoing queue."""

        yami4py.yami4_agent_close_connection(
            self.__agent, _utf8(target), priority)


class ValuePublisher(object):
    """Simple subscription publisher.
    
    The subscription publisher that notifies remote listeners
    with published value updates.
    
    Remote listeners can subscribe and unsubscribe at any time."""

    # possible decisions in the case of notification overflow
    WAIT_FOR_PREVIOUS_MESSAGE = 1
    ABANDON_MESSAGE = 2
    ABANDON_SUBSCRIPTION = 3

    class __DefaultOverflowCommand(object):
        def __call__(self, server_name, object_name, value):
            return ValuePublisher.WAIT_FOR_PREVIOUS_MESSAGE

    class __SubscriptionInfo(object): pass

    def __init__(self, user_command = None,
                 max_queue_length = 1,
                 overflow_command = __DefaultOverflowCommand()):
        """Constructor.
        
        Creates the subscription publisher
        that is not registered at any agent and that has a handler
        for arbitrary remote commands.
        
        Note:
        The "subscribe" and "unsubscribe" messages are also forwarded
        to the user-provided callback, but these two messages are already
        processed by the publisher's implementation."""

        self.__lock = threading.Lock()
        self.__controlling_agent = None
        self.__subscriptions = {}
        self.__user_command = user_command
        self.__max_queue_length = max_queue_length
        self.__overflow_command = overflow_command

    def register_at(self, agent, object_name):
        """Registers the publisher at the given agent."""

        if self.__controlling_agent != None:
            raise YAMIError(
                "Cannot register the same value publisher " +
                "with two different agents.")
        agent.register_object(object_name, self)
        self.__controlling_agent = agent
        self.__object_name = object_name

    def unregister(self):
        """Unregisters the publisher from its associated agent."""

        if self.__controlling_agent == None:
            raise YAMIError("This value publisher is not registered.")
        self.__controlling_agent.unregister(self.__object_name)
        self.__controlling_agent = None

    def __call__(self, message):
        message_name = message.get_message_name()

        if message_name == "subscribe" or message_name == "unsubscribe":
            # extract the destination target

            content = message.get_parameters()
            if "destination_target" in content:
                destination_target = content["destination_target"]
            else:
                # if the destination target is not specified
                # in the subscription message, use the
                # message source as a default
                destination_target = message.get_source()

            if message_name == "subscribe":
                # extract the destination object name

                if "destination_object" in content:
                    destination_object = content["destination_object"]
                else:
                    # if the destination object is not specified
                    # in the subscription message, use the
                    # local object name as a default
                    destination_object = message.get_object_name()

                self.subscribe(destination_target, destination_object)

            else: # message_name == "unsubscribe"
                self.unsubscribe(destination_target)

        # any message - delegate to user
        if self.__user_command != None:
            self.__user_command(message)
        else:
            # in the absence of user command, just confirm this operation
            message.reply()

    def subscribe(self, destination_target, destination_object):
        """Subscribes the new listener.
        
        This function is usually called internally as a result of
        processing the remote "subscribe" message, but can be also
        used locally if the listener's location is obtained via
        other means."""

        self.__lock.acquire()
        try:
            if destination_target in self.__subscriptions:
                # there is already a subscription for this target
                # -> refresh it

                sub_info = self.__subscriptions[destination_target]
                
                self.__release_last_message(sub_info)
                sub_info.destination_object = destination_object

            else:
                # this is a new subscription

                # make sure the channel exists,
                # so that further sends will not have to create it

                self.__controlling_agent.open_connection(destination_target)

                sub_info = ValuePublisher.__SubscriptionInfo()
                sub_info.destination_object = destination_object
                sub_info.last_messages = []
                self.__subscriptions[destination_target] = sub_info
        finally:
            self.__lock.release()
                
    def unsubscribe(self, destination_target):
        """Unsubscribes the given listener."""

        self.__lock.acquire()
        try:
            if destination_target in self.__subscriptions:
                self.__release_last_messages(
                    self.__subscriptions[destination_target])
                del self.__subscriptions[destination_target]
        finally:
            self.__lock.release()

    def publish(self, value, priority = 0):
        """Publishes the new value.
        
        Sends the update message to all active listeners with the given value.
        In case of any errors or communication problems, the problematic
        listener is automatically unsubscribed."""

        if self.__controlling_agent == None:
            raise YAMIError("This publisher is not registered.")

        self.__lock.acquire()
        try:
            keys_to_remove = []
            for sub in self.__subscriptions.items():
                destination_target = sub[0]
                sub_info = sub[1]
                destination_object = sub_info.destination_object

                # check all previous messages that were still not processed
                # by this subscriber

                abandon_subscription = False

                new_last_messages = []
                for msg in sub_info.last_messages:
                    state = msg.get_state()[0]
                    if state == OutgoingMessage.TRANSMITTED or \
                            state == OutgoingMessage.REPLIED or \
                            state == OutgoingMessage.REJECTED:

                        # this previous message has been successfully sent

                        msg.close()

                    elif state == OutgoingMessage.ABANDONED:

                        # the whole channel is broken
                        # - abandon the subscription

                        msg.close()
                        abandon_subscription = True

                    else:
                        # message still not yet transmitted,
                        # keep it for the next cycle
                        new_last_messages.append(msg)

                    if abandon_subscription:
                        break

                sub_info.last_messages = new_last_messages

                if abandon_subscription:
                    keys_to_remove.append(destination_target)
                    continue

                # check if there is a place in the queue

                if len(sub_info.last_messages) >= self.__max_queue_length:

                    # the queue is full - ask user for decision

                    decision = ValuePublisher.WAIT_FOR_PREVIOUS_MESSAGE
                    try:
                        decision = self.__overflow_command(
                            destination_target,
                            destination_object,
                            value)
                    except:
                        # threat user exceptions as "abandon message"
                        decision = ValuePublisher.ABANDON_MESSAGE

                    if decision == ValuePublisher.WAIT_FOR_PREVIOUS_MESSAGE:

                        msg = sub_info.last_messages[0]
                        msg.wait_for_transmission()
                        msg.close()
                        sub_info.last_messages.pop(0)

                    elif decision == ValuePublisher.ABANDON_MESSAGE:

                        continue

                    else: # decision == ValuePublisher.ABANDON_SUBSCRIPTION
                        
                        keys_to_remove.append(destination_target)

                # send the message

                try:
                    auto_connect = False
                    msg = self.__controlling_agent.send(
                        destination_target, destination_object,
                        "subscription_update", value, priority,
                        auto_connect)

                    sub_info.last_messages.append(msg)

                except:
                    # in case of any error drop this subscription
                    keys_to_remove.append(destination_target)

            for k in keys_to_remove:
                self.__release_last_messages(self.__subscriptions[k])
                del self.__subscriptions[k]
        finally:
            self.__lock.release()

    def get_number_of_subscribers(self):
        """Returns the number of active subscribers."""

        self.__lock.acquire()
        try:
            return len(self.__subscriptions)
        finally:
            self.__lock.release()

    def get_subscribers(self):
        """Returns the information about all active subscribers.
        
        The first component of each vector entry is a destination target
        and the second component is a destination object for
        the given subscriber."""

        result = []
        self.__lock.acquire()
        try:
            for sub in self.__subscriptions.items():
                destination_target = sub[0]
                destination_object = sub[1].destination_object
                result.append((destination_target, destination_object))
        finally:
            self.__lock.release()

        return result

    # synchronized by caller
    def __release_last_messages(self, sub):
        for msg in sub.last_messages:
            msg.close()
        sub.last_messages = []


VERSION_NAME = "1.9.0"
VERSION_NUMBER = 10900

# initialization

if sys.platform == "darwin":
    native_file_name = "libyami4py.dylib"
elif sys.platform == "win32":
    native_file_name = "yami4py.dll"
else:
    native_file_name = "libyami4py.so"

if _use_standard_extension_API:
    full_file_path = os.path.join(os.path.dirname(__file__), native_file_name)
    file = open(full_file_path)
    try:
        imp.load_module("yami4py", file, full_file_path, ('.so', 'rb', imp.C_EXTENSION))
    finally:
        file.close()
    import yami4py
else:
    yami4py = ctypes.CDLL(native_file_name)

    yami4py.yami4_is_success.restype = ctypes.c_int
    yami4py.yami4_get_pointer.restype = ctypes.c_void_p
    yami4py.yami4_get_int_i.restype = ctypes.c_int
    yami4py.yami4_get_int_j.restype = ctypes.c_int
    yami4py.yami4_get_int_k.restype = ctypes.c_int
    yami4py.yami4_get_string.restype = ctypes.c_char_p
    yami4py.yami4_get_error.restype = ctypes.c_char_p
    yami4py.yami4_destroy_result.restype = None
    yami4py.yami4_create_agent.restype = ctypes.c_void_p
    yami4py.yami4_agent_terminate_incoming_queue.restype = None
    yami4py.yami4_destroy_agent.restype = None
    yami4py.yami4_agent_add_listener.restype = ctypes.c_void_p
    yami4py.yami4_agent_remove_listener.restype = None
    yami4py.yami4_agent_open_connection.restype = ctypes.c_void_p
    yami4py.yami4_agent_send.restype = ctypes.c_void_p
    yami4py.yami4_agent_send_one_way.restype = ctypes.c_void_p
    yami4py.yami4_agent_get_next_incoming_message.restype = ctypes.c_void_p
    yami4py.yami4_agent_get_next_connection_event.restype = ctypes.c_void_p
    yami4py.yami4_agent_close_connection.restype = None
    yami4py.yami4_outgoing_message_get_state.restype = ctypes.c_void_p
    yami4py.yami4_outgoing_message_wait_for_transmission.restype = ctypes.c_void_p
    yami4py.yami4_outgoing_message_wait_for_completion.restype = ctypes.c_void_p
    yami4py.yami4_outgoing_message_get_raw_reply.restype = ctypes.c_void_p
    yami4py.yami4_outgoing_message_get_exception_msg.restype = ctypes.c_void_p
    yami4py.yami4_destroy_outgoing_message.restype = None
    yami4py.yami4_incoming_message_get_source.restype = ctypes.c_void_p
    yami4py.yami4_incoming_message_get_object_name.restype = ctypes.c_void_p
    yami4py.yami4_incoming_message_get_message_name.restype = ctypes.c_void_p
    yami4py.yami4_incoming_message_get_raw_content.restype = ctypes.c_void_p
    yami4py.yami4_incoming_message_reply.restype = None
    yami4py.yami4_incoming_message_reject.restype = None
    yami4py.yami4_destroy_incoming_message.restype = None
