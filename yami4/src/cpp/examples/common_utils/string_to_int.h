#ifndef YAMICPP_STRING_TO_INT_H_INCLUDED
#define YAMICPP_STRING_TO_INT_H_INCLUDED

#include <cstdlib>

namespace examples
{

// helper function for convering string to int
bool string_to_int(const char * str, int & value)
{
    bool result;
    char * endptr;
    const long tmp = std::strtol(str, &endptr, 10);
    if (endptr != str && *endptr == '\0')
    {
        value = static_cast<int>(tmp);
        result = true;
    }
    else
    {
        result = false;
    }

    return result;
}

} // namespace examples

#endif // YAMICPP_STRING_TO_INT_H_INCLUDED
