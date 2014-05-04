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

#import "Parameters.h"
#import "Exception.h"

#include <yami4-cpp/parameters.h>

#include <cassert>
#include <sstream>


// helper function, translates parameter type enumerations
enum YAMI4ParameterType translateType(yami::parameter_type t)
{
    switch (t)
    {
    case yami::boolean:
        return YAMI4Boolean;
    case yami::integer:
        return YAMI4Integer;
    case yami::long_long:
        return YAMI4LongLong;
    case yami::double_float:
        return YAMI4Double;
    case yami::string:
        return YAMI4String;
    case yami::binary:
        return YAMI4Binary;
    case yami::boolean_array:
        return YAMI4BooleanArray;
    case yami::integer_array:
        return YAMI4IntegerArray;
    case yami::long_long_array:
        return YAMI4LongLongArray;
    case yami::double_float_array:
        return YAMI4DoubleArray;
    case yami::string_array:
        return YAMI4StringArray;
    case yami::binary_array:
        return YAMI4BinaryArray;
    case yami::nested_parameters:
        return YAMI4NestedParameters;
    case yami::nested_parameters_array:
        return YAMI4NestedParametersArray;
    default:
        assert(0);
    }
}

@implementation YAMI4Parameters

+(YAMI4Parameters *)new
{
    YAMI4Parameters * newObject = [[YAMI4Parameters alloc] init];
    return newObject;
}

-(YAMI4Parameters *)init
{
    self = [super init];
    if (self != nil)
    {
        params_ = new yami::parameters();
        variable_ = YES;
        owner_ = YES;
    }

    return self;
}

-(YAMI4Parameters *)initWithParameters:(YAMI4Parameters *) params
{
    yami::parameters * p =
        static_cast<yami::parameters *>([params getInternals]);
        
    self = [super init];
    if (self != nil)
    {
        params_ = new yami::parameters(*p);
        variable_ = YES;
        owner_ = YES;
    }

    return self;
}

-(YAMI4Parameters *)initWithObject:(void *)p
    variable:(BOOL) variable owner:(BOOL) owner
{
    self = [super init];
    if (self != nil)
    {
        params_ = p;
        variable_ = variable;
        owner_ = owner;
    }

    return self;
}

-(void)dealloc
{
    if (owner_)
    {
        yami::parameters * p = static_cast<yami::parameters *>(params_);
        delete p;
    }

    [super dealloc];
}

-(void)setBoolean:(NSString *)name value:(BOOL)value
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->set_boolean([name UTF8String], value);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(BOOL)getBoolean:(NSString *)name
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        return p->get_boolean([name UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return NO;
}

-(void)setInteger:(NSString *)name value:(int)value
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->set_integer([name UTF8String], value);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(int)getInteger:(NSString *)name
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        return p->get_integer([name UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return 0;
}

-(void)setLongLong:(NSString *)name value:(long long)value
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->set_long_long([name UTF8String], value);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(long long)getLongLong:(NSString *)name
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        return p->get_long_long([name UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return 0;
}

-(void)setDouble:(NSString *)name value:(double)value
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->set_double_float([name UTF8String], value);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(double)getDouble:(NSString *)name
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        return p->get_double_float([name UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return 0.0;
}

-(void)setString:(NSString *)name value:(NSString *)value
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->set_string([name UTF8String], [value UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(NSString *)getString:(NSString *)name
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        const std::string & str = p->get_string([name UTF8String]);
        NSString * ret = [[NSString alloc] initWithUTF8String:str.c_str()];
        
        return ret;
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return nil;
}

-(void)setBinary:(NSString *)name value:(const void *)value
    valueLength:(size_t)valueLength
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->set_binary([name UTF8String], value, valueLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(const void *)getBinary:(NSString *)name valueLength:(size_t *)valueLength
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        return p->get_binary([name UTF8String], *valueLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return NULL;
}

-(void)setBooleanArray:(NSString *)name values:(BOOL *)values
    arrayLength:(size_t) arrayLength
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    // convert BOOL to bool
    bool * cppValues = new bool[arrayLength];
    for (size_t i = 0; i != arrayLength; ++i)
    {
        cppValues[i] = static_cast<bool>(values[i]);
    }
    
    try
    {
        p->set_boolean_array([name UTF8String], cppValues, arrayLength);

        delete [] cppValues;
    }
    catch (const std::exception & e)
    {
        delete [] cppValues;

        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(BOOL *)getBooleanArray:(NSString *)name arrayLength:(size_t *) arrayLength
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    bool * cppValues;

    try
    {
        cppValues = p->get_boolean_array([name UTF8String], *arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    BOOL * values = static_cast<BOOL *>(malloc(*arrayLength * sizeof(BOOL)));
    
    for (size_t i = 0; i != *arrayLength; ++i)
    {
        values[i] = static_cast<BOOL>(cppValues[i]);
    }
    
    return values;
}

-(void)setIntegerArray:(NSString *)name values:(int *)values
    arrayLength:(size_t) arrayLength
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->set_integer_array([name UTF8String], values, arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}
    
-(int *)getIntegerArray:(NSString *)name arrayLength:(size_t *) arrayLength
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        return p->get_integer_array([name UTF8String], *arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return NULL;
}

-(void)setLongLongArray:(NSString *)name values:(long long *)values
    arrayLength:(size_t) arrayLength
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->set_long_long_array([name UTF8String], values, arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}
    
-(long long *)getLongLongArray:(NSString *)name arrayLength:(size_t *) arrayLength
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        return p->get_long_long_array([name UTF8String], *arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return NULL;
}

-(void)setDoubleArray:(NSString *)name values:(double *)values
    arrayLength:(size_t) arrayLength
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->set_double_float_array([name UTF8String], values, arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}
    
-(double *)getDoubleArray:(NSString *)name arrayLength:(size_t *) arrayLength
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        return p->get_double_float_array([name UTF8String], *arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return NULL;
}

-(void)createStringArray:(NSString *)name arrayLength:(size_t) arrayLength
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->create_string_array([name UTF8String], arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)setStringInArray:(NSString *)name
    index:(size_t)index value:(NSString *)value
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->set_string_in_array([name UTF8String], index, [value UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(size_t)getStringArrayLength:(NSString *)name
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        return p->get_string_array_length([name UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return 0;
}

-(NSString *)getStringInArray:(NSString *)name index:(size_t)index
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        const std::string & str = p->get_string_in_array([name UTF8String], index);
        NSString * ret = [[NSString alloc] initWithUTF8String:str.c_str()];
        
        return ret;
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return nil;
}

-(void)createBinaryArray:(NSString *)name arrayLength:(size_t) arrayLength
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->create_binary_array([name UTF8String], arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)setBinaryInArray:(NSString *)name
    index:(size_t)index value:(const void *)value
    valueLength:(size_t)valueLength
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->set_binary_in_array([name UTF8String], index, value, valueLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(size_t)getBinaryArrayLength:(NSString *)name
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        return p->get_binary_array_length([name UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return 0;
}

-(const void *)getBinaryInArray:(NSString *)name index:(size_t)index
    valueLength:(size_t *)valueLength
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        return p->get_binary_in_array(
            [name UTF8String], index, *valueLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return NULL;
}

-(YAMI4Parameters *)createNestedParameters:(NSString *)name
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        yami::parameters * nested =
            new yami::parameters(
                p->create_nested_parameters([name UTF8String]));

        YAMI4Parameters * ret =
            [[YAMI4Parameters alloc] initWithObject:nested
                variable:YES owner:YES];

        if (ret != nil)
        {
            return ret;
        }
        else
        {
            delete nested;
        }
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // normally unreachable
    return nil;
}

-(YAMI4Parameters *)getNestedParameters:(NSString *)name
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        yami::parameters * nested =
            new yami::parameters(
                p->get_nested_parameters([name UTF8String]));

        YAMI4Parameters * ret =
            [[YAMI4Parameters alloc] initWithObject:nested
                variable:YES owner:YES];

        if (ret != nil)
        {
            return ret;
        }
        else
        {
            delete nested;
        }
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // normally unreachable
    return nil;
}

-(void)createNestedArray:(NSString *)name arrayLength:(size_t) arrayLength
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        p->create_nested_array([name UTF8String], arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(size_t)getNestedArrayLength:(NSString *)name
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        return p->get_nested_array_length([name UTF8String]);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return 0;
}

-(YAMI4Parameters *)getNestedInArray:(NSString *)name index:(size_t)index
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        yami::parameters * nested =
            new yami::parameters(
                p->get_nested_in_array([name UTF8String], index));

        YAMI4Parameters * ret =
            [[YAMI4Parameters alloc] initWithObject:nested
                variable:YES owner:YES];

        if (ret != nil)
        {
            return ret;
        }
        else
        {
            delete nested;
        }
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return NULL;
}

-(size_t)size
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    return p->size();
}

-(enum YAMI4ParameterType)type:(NSString *)name
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    try
    {
        yami::parameter_type t = p->type([name UTF8String]);

        return translateType(t);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return YAMI4Boolean;
}

-(YAMI4ParametersIterator *)begin
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    void * initialValue = new yami::parameters::iterator(p->begin());
    YAMI4ParametersIterator * it =
        [[YAMI4ParametersIterator alloc] initWithValue:initialValue];

    return it;
}

-(YAMI4ParametersIterator *)end
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    void * initialValue = new yami::parameters::iterator(p->end());
    YAMI4ParametersIterator * it =
        [[YAMI4ParametersIterator alloc] initWithValue:initialValue];

    return it;
}

-(BOOL)find:(NSString *)name entry:(YAMI4ParameterEntry * *)entry
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    const char * cName = [name UTF8String];

    yami::parameter_entry * newEntry = new yami::parameter_entry();

    bool found = p->find(cName, *newEntry);

    if (found)
    {
        *entry = [[YAMI4ParameterEntry alloc] initWithObject:newEntry];
    }
    else
    {
        delete newEntry;
    }

    return found;
}

-(void)remove:(NSString *)name
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    const char * cName = [name UTF8String];

    try
    {
        p->remove(cName);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)removeAt:(YAMI4ParametersIterator *)it
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);
    
    yami::parameters::iterator * where =
        static_cast<yami::parameters::iterator *>([it getInternals]);
    
    p->remove(*where);
}

-(void)mergeFrom:(YAMI4Parameters *)other
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    yami::parameters * op =
        static_cast<yami::parameters *>([other getInternals]);

    try
    {
        p->merge_from(*op);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
}

-(void)clear
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    p->clear();
}

-(size_t)serializeBufferSize
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    return p->serialize_buffer_size();
}

-(void)serialize:(char * *)buffers
     bufferSizes:(size_t *)bufferSizes numOfBuffers:(size_t)numOfBuffers
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    p->serialize(buffers, bufferSizes, numOfBuffers);
}

-(void)deserialize:(const char * *)buffers
     bufferSizes:(size_t *)bufferSizes numOfBuffers:(size_t)numOfBuffers
{
    if (variable_ == NO)
    {
        [YAMI4Exception raiseFrom:"This parameters object is immutable"];
    }
    
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    p->deserialize(buffers, bufferSizes, numOfBuffers);
}

-(void *)getInternals
{
    return params_;
}

-(NSString *)dump
{
    yami::parameters * p = static_cast<yami::parameters *>(params_);

    std::ostringstream ss;

    try
    {
        p->dump(ss);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    NSString * ret = [[NSString alloc] initWithUTF8String:ss.str().c_str()];

    return ret;
}

@end


@implementation YAMI4ParameterEntry

-(YAMI4ParameterEntry *)initWithObject:(void *)e
{
    self = [super init];
    if (self != nil)
    {
        e_ = e;
    }

    return self;
}

-(void)dealloc
{
    yami::parameter_entry * e = static_cast<yami::parameter_entry *>(e_);

    delete e;

    [super dealloc];
}

-(enum YAMI4ParameterType)type
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    yami::parameter_type t = e->type();
    
    return translateType(t);
}

-(NSString *)name
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    const std::string & eName = e->name();
    
    NSString * ret = [[NSString alloc] initWithUTF8String:eName.c_str()];
    
    return ret;
}

-(BOOL)getBoolean
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        return e->get_boolean();
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // dummy unreachable
    return NO;
}

-(int)getInteger
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        return e->get_integer();
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return 0;
}

-(long long)getLongLong
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        return e->get_long_long();
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return 0;
}

-(double)getDouble
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        return e->get_double_float();
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return 0.0;
}

-(NSString *)getString
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        const std::string & str = e->get_string();
        NSString * ret = [[NSString alloc] initWithUTF8String:str.c_str()];
        
        return ret;
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return nil;
}

-(const void *)getBinary:(size_t *)valueLength
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        return e->get_binary(*valueLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return NULL;
}

-(BOOL *)getBooleanArray:(size_t *) arrayLength
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    bool * cppValues;
    
    try
    {
        cppValues = e->get_boolean_array(*arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    BOOL * values = static_cast<BOOL *>(malloc(*arrayLength * sizeof(BOOL)));
    
    for (size_t i = 0; i != *arrayLength; ++i)
    {
        values[i] = static_cast<BOOL>(cppValues[i]);
    }
    
    return values;
}

-(int *)getIntegerArray:(size_t *) arrayLength
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        return e->get_integer_array(*arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return NULL;
}

-(long long *)getLongLongArray:(size_t *) arrayLength
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        return e->get_long_long_array(*arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return NULL;
}

-(double *)getDoubleArray:(size_t *) arrayLength
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        return e->get_double_float_array(*arrayLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return NULL;
}

-(size_t)getStringArrayLength
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        return e->get_string_array_length();
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return 0;
}

-(NSString *)getStringInArray:(size_t)index
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        const std::string & str = e->get_string_in_array(index);
        NSString * ret = [[NSString alloc] initWithUTF8String:str.c_str()];
        
        return ret;
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return nil;
}

-(size_t)getBinaryArrayLength
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        return e->get_binary_array_length();
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return 0;
}

-(const void *)getBinaryInArray:(size_t)index
    valueLength:(size_t *)valueLength
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        return e->get_binary_in_array(index, *valueLength);
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }

    // dummy unreachable
    return NULL;
}

-(YAMI4Parameters *)getNestedParameters
{
    yami::parameter_entry * e =
        static_cast<yami::parameter_entry *>(e_);

    try
    {
        yami::parameters * nested =
            new yami::parameters(e->get_nested_parameters());

        YAMI4Parameters * ret =
            [[YAMI4Parameters alloc] initWithObject:nested
                variable:YES owner:YES];

        if (ret != nil)
        {
            return ret;
        }
        else
        {
            delete nested;
        }
    }
    catch (const std::exception & e)
    {
        [YAMI4Exception raiseFrom:e.what()];
    }
    
    // normally unreachable
    return nil;
}

@end


@implementation YAMI4ParametersIterator

-(YAMI4ParametersIterator *)initWithValue:(void *)p
{
    self = [super init];
    if (self != nil)
    {
        it_ = p;
    }

    return self;
}

-(void *)getInternals
{
    return it_;
}

-(void)dealloc
{
    yami::parameters::iterator * it = static_cast<yami::parameters::iterator *>(it_);

    delete it;

    [super dealloc];
}

-(void)next
{
    yami::parameters::iterator * it = static_cast<yami::parameters::iterator *>(it_);

    ++(*it);
}

-(BOOL)isEqual:(YAMI4ParametersIterator *)other
{
    yami::parameters::iterator * it =
        static_cast<yami::parameters::iterator *>(it_);
    yami::parameters::iterator * otherIt =
        static_cast<yami::parameters::iterator *>(other->it_);

    return *it == *otherIt;
}

-(YAMI4ParameterEntry *)entry
{
    yami::parameters::iterator * it =
        static_cast<yami::parameters::iterator *>(it_);

    yami::parameter_entry * e = new yami::parameter_entry(**it);
	
    YAMI4ParameterEntry * newEntry =
        [[YAMI4ParameterEntry alloc] initWithObject:e];
    
    return newEntry;
}

@end
