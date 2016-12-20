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

#include <assert.h>

using namespace patterns;

int main()
{
    assert(patterns_multi_hierarchic_match("abc", "abc") == 1);
    assert(patterns_multi_hierarchic_match("abc", "abcc") == 0);
    assert(patterns_multi_hierarchic_match("abc", "ab") == 0);
    assert(patterns_multi_hierarchic_match("abc", "abd") == 0);
    assert(patterns_multi_hierarchic_match("abc", "*") == 1);
    assert(patterns_multi_hierarchic_match("abc.klm", "abc.klm") == 1);
    assert(patterns_multi_hierarchic_match("abc.klm", "abc.kln") == 0);
    assert(patterns_multi_hierarchic_match("abc.klm", "abcc.kln") == 0);
    assert(patterns_multi_hierarchic_match("abc.klm", "ab.kln") == 0);
    assert(patterns_multi_hierarchic_match("abc.klm", "abd.klm") == 0);
    assert(patterns_multi_hierarchic_match("abc.klm", "abc.*") == 1);
    assert(patterns_multi_hierarchic_match("abc.klm", "abcc.*") == 0);
    assert(patterns_multi_hierarchic_match("abc.klm", "ab.*") == 0);
    assert(patterns_multi_hierarchic_match("abc.klm", "abd.*") == 0);
    assert(patterns_multi_hierarchic_match("abc.klm", "*") == 1);
    assert(patterns_multi_hierarchic_match("abc.klm", "*.klm") == 1);
    assert(patterns_multi_hierarchic_match("abc.klm", "*.kln") == 0);
    assert(patterns_multi_hierarchic_match("abc.klm.xyz", "*.klm.xyz") == 1);
    assert(patterns_multi_hierarchic_match("abc.klmm.xyz", "*.klm.xyz") == 0);
    assert(patterns_multi_hierarchic_match("abc.klm.xyzz", "*.klm.xyz") == 0);
    assert(patterns_multi_hierarchic_match("abc.klm.xyz", "abc.*.xyz") == 1);
    assert(patterns_multi_hierarchic_match("abcc.klm.xyz", "abc.*.xyz") == 0);
    assert(patterns_multi_hierarchic_match("abc.klm.xyzz", "abc.*.xyz") == 0);
    assert(patterns_multi_hierarchic_match("abc.klm.xyz", "abc.klm.*") == 1);
    assert(patterns_multi_hierarchic_match("abcc.klm.xyz", "abc.klm.*") == 0);
    assert(patterns_multi_hierarchic_match("abc.klmm.xyz", "abc.klm.*") == 0);
    assert(patterns_multi_hierarchic_match("x,y", "x") == 1);
    assert(patterns_multi_hierarchic_match("x,y", "y") == 1);
    assert(patterns_multi_hierarchic_match("x,y", "z") == 0);
    assert(patterns_multi_hierarchic_match("x,y", "x,z") == 1);
    assert(patterns_multi_hierarchic_match("x,y", "z,x") == 1);
    assert(patterns_multi_hierarchic_match("x,y", "y,z") == 1);
    assert(patterns_multi_hierarchic_match("x,y", "z,y") == 1);
    assert(patterns_multi_hierarchic_match("x,y", "x,z") == 1);
    assert(patterns_multi_hierarchic_match("x,y", "*") == 1);
    assert(patterns_multi_hierarchic_match("x,y", "*,z") == 1);
    assert(patterns_multi_hierarchic_match("x,y", "z,*") == 1);
    assert(patterns_multi_hierarchic_match("x,y", "k,l") == 0);
}
