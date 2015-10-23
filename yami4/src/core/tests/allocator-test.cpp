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

#include "../allocator.h"
#include <algorithm>
#include <cstdlib>
#include <ctime>
#include <cassert>
#include <vector>

// series of allocs, out of order dealloc
void test1()
{
    const std::size_t private_area_size = 80000;
    char area[private_area_size];

    yami::details::allocator a;
    a.set_working_area(area, private_area_size);

    const int num_of_blocks = 1000;
    std::vector<void *> blocks;
    for (int i = 0; i != num_of_blocks; ++i)
    {
        void * p = a.allocate(10);
        assert(p != NULL);
        if (blocks.empty() == false)
        {
            // check that the blocks are ordered in the working area
            assert(p > blocks[blocks.size() - 1]);
        }
        blocks.push_back(p);
    }

    std::random_shuffle(blocks.begin(), blocks.end());

    for (int i = 0; i != num_of_blocks; ++i)
    {
        a.deallocate(blocks[i]);
    }
}

// mixed and out of order alloc/dealloc
void test2()
{
    char area[100000];

    yami::details::allocator a;
    a.set_working_area(area, 100000);

    const int num_of_steps = 1000000;
    std::vector<void *> blocks;

    std::srand(static_cast<unsigned int>(std::time(NULL)));
    for (int i = 0; i != num_of_steps; ++i)
    {
        enum action { alloc, dealloc };
        action to_do;

        std::size_t current_blocks = blocks.size();

        if (current_blocks < 100)
        {
            to_do = alloc;
        }
        else if (current_blocks == 1000)
        {
            to_do = dealloc;
        }
        else
        {
            if ((std::rand() & 1) == 0)
            {
                to_do = alloc;
            }
            else
            {
                to_do = dealloc;
            }
        }

        if (to_do == alloc)
        {
            void * p = a.allocate(10);
            assert(p != NULL);
            blocks.push_back(p);
        }
        else
        {
            std::size_t index = std::rand() % current_blocks;
            a.deallocate(blocks[index]);
            blocks.erase(blocks.begin() + index);
        }
    }

    // deallocate remaining blocks
    for (std::size_t i = 0; i != blocks.size(); ++i)
    {
        a.deallocate(blocks[i]);
    }
}

int main()
{
    test1();
    test2();
}
