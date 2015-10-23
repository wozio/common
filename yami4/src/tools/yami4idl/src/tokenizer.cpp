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

#include "tokenizer.h"

#include "idl.h"
#include "parser.h"

#include <cctype>

#include <cstdio>
using namespace tokenizer;

namespace // unnamed
{

enum tokenizer_state_type { whitespace, comment, text };

tokenizer_state_type tokenizer_state_ = whitespace;

std::string acc_;

bool is_special(char c)
{
    return (c == '(') || (c == ')') || (c == ':') || (c == ';');
}

} // unnamed namespace

void tokenizer::reset()
{
    tokenizer_state_ = whitespace;
    acc_.clear();
}

void tokenizer::tokenize(char c)
{
    switch (tokenizer_state_)
    {
    case whitespace:

        if (c == '-')
        {
            // in this simple grammar
            // one minus is enough to indicate comment
            tokenizer_state_ = comment;
        }
        else if (std::isalpha(c))
        {
            acc_ += c;
            tokenizer_state_ = text;
        }
        else if (is_special(c))
        {
            parser::process_token(std::string(1, c));
        }
        else if (isspace(c))
        {
            // nothing to do
        }
        else
        {
            throw idl::invalid_input_error("unrecognized character");
        }

        break;

    case comment:

        if (c == '\n')
        {
            tokenizer_state_ = whitespace;
        }

        break;

    case text:

        if (isalnum(c) || (c == '_') || (c == '.'))
        {
            acc_ += c;
        }
        else if (is_special(c) || isspace(c))
        {
            tokenizer_state_ = whitespace;

            parser::process_token(acc_);
            acc_.clear();

            if (is_special(c))
            {
                parser::process_token(std::string(1, c));
            }
        }
        else if (c == '-')
        {
            parser::process_token(acc_);
            acc_.clear();

            tokenizer_state_ = comment;
        }
        else
        {
            throw idl::invalid_input_error("unrecognized character");
        }

        break;
    }
}

std::size_t tokenizer::last_accumulated_length()
{
    return acc_.size();
}
