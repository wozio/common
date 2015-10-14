// Copyright Maciej Sobczak 2008-2015.
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

#include "patterns.h"

#include <cstring>
#include <vector>

using namespace patterns;

namespace // unnamed
{

const char patterns_comma = ',';
const char patterns_dot = '.';
const char patterns_star = '*';

// helper for single string vs. single pattern matching
bool patterns_hierarchic_match(const char str[], const char pat[])
{
    int result = -1; // unknown

    std::size_t si = 0u;
    std::size_t pi = 0u;

    while ((result == -1) &&
        (str[si] != '\0') && (pat[pi] != '\0'))
    {
        if (pat[pi] == patterns_star)
        {
            // skip characters in strings until dot or end
            while ((str[si] != '\0') && (str[si] != patterns_dot))
            {
                ++si;
            }

            // if star is the last character in the pattern,
            // then we consider the rest of strings to match
            // (this is the "hierarchic" part of matching)
            
            if (pat[pi + 1u] == '\0')
            {
                result = 1; // true
            }
            else
            {
                // otherwise move on with the pattern,
                // there are more components to check
                ++pi;

            }
        }
        else if (str[si] != pat[pi])
        {
            result = 0; // false
        }
        else
        {
            ++si;
            ++pi;
        }
    }
    
    if (result == -1)
    {
        if ((str[si] == '\0') && (pat[pi] == '\0'))
        {
            result = 1; // true
        }
        else
        {
            result = 0; // false
        }
    }

    return result != 0;
}

} // unnamed namespace

bool patterns::patterns_multi_hierarchic_match(
    const std::string & tags, const std::string & patterns)
{
    bool result = false; // unless match is found

    if (tags == patterns)
    {
        // in the simple (and most frequent) case
        // there is only one tag and one pattern, so it can be optimized away

        result = true;
    }
    else
    {
        std::vector<char> tags2(tags.size() + 1);
        std::strcpy(&tags2[0], tags.c_str());

        std::vector<char> patterns2(patterns.size() + 1);
        std::strcpy(&patterns2[0], patterns.c_str());

        // divide strings and patterns into single tokens
        // by just injecting zeros in place of separators
        for (std::size_t i = 0u; i != tags2.size(); ++i)
        {
            if (tags2[i] == patterns_comma)
            {
                tags2[i] = '\0';
            }
        }

        for (std::size_t j = 0u; j != patterns2.size(); ++j)
        {
            if (patterns2[j] == patterns_comma)
            {
                patterns2[j] = '\0';
            }
        }
    
        // try to match every tag with every pattern
        // - at least one match means the whole is accepted
    
        std::size_t j = 0u; // index for patterns
        while (j < patterns2.size())
        {
            std::size_t i = 0u; /* index for tags */
            while (i < tags2.size())
            {
                bool single_result = patterns_hierarchic_match(
                    &tags2[i], &patterns2[j]);

                if (single_result)
                {
                    result = true;

                    break;
                }
                else
                {
                    // move to the next tag in strings
                    while (tags2[i] != '\0')
                    {
                        ++i;
                    }

                    ++i;
                }
            }

            if (result)
            {
                // pattern matches

                break;
            }

            // move to the next pattern
            while (patterns2[j] != '\0')
            {
                ++j;
            }

            ++j;
        }
    }
    
    return result;
}
