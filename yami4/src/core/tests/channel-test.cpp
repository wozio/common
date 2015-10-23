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
#include "../channel.h"
#include "../incoming_frame.h"
#include "../options.h"
#include "../outgoing_frame.h"
#include <cassert>
#include <cstring>
#include <string>
#include <fstream>
#include <vector>
#include <algorithm>
#include <utility>

// selected per platform
#include <mutex.h>

// dummy channel test
void test1()
{
    yami::details::allocator a;
    yami::details::mutex mtx;
    yami::details::channel ch;

    mtx.init();

    yami::details::options default_options;
    default_options.init(NULL);
    yami::core::result res = ch.init(a, mtx, default_options, NULL,
        "abc", NULL, NULL, NULL, NULL, NULL, NULL);
    assert(res == yami::core::bad_protocol);

    mtx.clean();
}

// node insertion
void test2()
{
    yami::details::allocator a;
    yami::details::mutex mtx;
    yami::details::channel ch;

    mtx.init();

    yami::details::options default_options;
    default_options.init(NULL);
    yami::core::result res = ch.init(a, mtx, default_options, NULL,
        "file://output.bin", NULL, NULL, NULL, NULL, NULL, NULL);
    assert(res == yami::core::ok);

    assert(ch.get_first_outgoing_frame() == NULL);
    assert(ch.get_last_outgoing_frame() == NULL);

    yami::details::outgoing_frame * f;

    bool first_frame;

    char * buffers[1];
    std::size_t buffer_sizes[1];
    buffers[0] = NULL;
    buffer_sizes[0] = 123;
    res = ch.post(0, buffers, buffer_sizes, 1, first_frame, NULL, NULL);
    assert(res == yami::core::ok);
    assert(first_frame);
    f = ch.get_first_outgoing_frame();
    assert(f->size == 123);
    assert(f->next == NULL);
    assert(ch.get_last_outgoing_frame() == f);

    buffers[0] = NULL;
    buffer_sizes[0] = 456;
    // added at the end:
    res = ch.post(0, buffers, buffer_sizes, 1, first_frame, NULL, NULL);
    assert(res == yami::core::ok);
    assert(first_frame == false);
    f = ch.get_first_outgoing_frame();
    assert(f->size == 123);
    f = f->next;
    assert(f->size == 456);
    assert(f->next == NULL);
    assert(ch.get_last_outgoing_frame() == f);

    buffers[0] = NULL;
    buffer_sizes[0] = 789;
    // added at the end
    res = ch.post(0, buffers, buffer_sizes, 1, first_frame, NULL, NULL);
    assert(res == yami::core::ok);
    assert(first_frame == false);
    f = ch.get_first_outgoing_frame();
    assert(f->size == 123);
    f = f->next;
    assert(f->size == 456);
    f = f->next;
    assert(f->size == 789);
    assert(f->next == NULL);
    assert(ch.get_last_outgoing_frame() == f);

    char * buffers_a1[3];
    std::size_t buffer_sizes_a1[3];
    buffers_a1[0] = NULL;
    buffers_a1[1] = NULL;
    buffers_a1[2] = NULL;
    buffer_sizes_a1[0] = 1;
    buffer_sizes_a1[1] = 2;
    buffer_sizes_a1[2] = 3;
    // added after first
    res = ch.post(3, buffers_a1, buffer_sizes_a1, 3, first_frame, NULL, NULL);
    assert(res == yami::core::ok);
    assert(first_frame == false);
    f = ch.get_first_outgoing_frame();
    assert(f->size == 123);
    f = f->next;
    assert(f->size == 1);
    f = f->next;
    assert(f->size == 2);
    f = f->next;
    assert(f->size == 3);
    f = f->next;
    assert(f->size == 456);
    f = f->next;
    assert(f->size == 789);
    assert(f->next == NULL);
    assert(ch.get_last_outgoing_frame() == f);

    char * buffers_a2[2];
    std::size_t buffer_sizes_a2[2];
    buffers_a2[0] = NULL;
    buffers_a2[1] = NULL;
    buffer_sizes_a2[0] = 4;
    buffer_sizes_a2[1] = 5;
    // added after previous frames
    res = ch.post(1, buffers_a2, buffer_sizes_a2, 2, first_frame, NULL, NULL);
    assert(res == yami::core::ok);
    assert(first_frame == false);
    f = ch.get_first_outgoing_frame();
    assert(f->size == 123);
    f = f->next;
    assert(f->size == 1);
    f = f->next;
    assert(f->size == 2);
    f = f->next;
    assert(f->size == 3);
    f = f->next;
    assert(f->size == 4);
    f = f->next;
    assert(f->size == 5);
    f = f->next;
    assert(f->size == 456);
    f = f->next;
    assert(f->size == 789);
    assert(f->next == NULL);
    assert(ch.get_last_outgoing_frame() == f);

    bool close_me = false;
    res = ch.post_close(2, close_me); // poison pill added after fa1 frames
    assert(res == yami::core::ok);
    assert(close_me == false); // there are output frames, no immediate close
    f = ch.get_first_outgoing_frame();
    assert(f->size == 123);
    f = f->next;
    assert(f->size == 1);
    f = f->next;
    assert(f->size == 2);
    f = f->next;
    assert(f->size == 3);
    f = f->next;
    assert(f->close_flag); // poison pill
    assert(f->next == NULL);
    assert(ch.get_last_outgoing_frame() == f);

    ch.clean();

    yami::details::channel ch2;

    res = ch2.init(a, mtx, default_options, NULL,
        "file://output.bin", NULL, NULL, NULL, NULL, NULL, NULL);
    assert(res == yami::core::ok);

    res = ch2.post_close(2, close_me); // no poison pill added
    assert(res == yami::core::ok);
    assert(close_me); // there are no output frames, immediate close
    assert(ch2.get_first_outgoing_frame() == NULL);
    assert(ch2.get_last_outgoing_frame() == NULL);

    ch2.clean();
}

// file output
void test3()
{
    yami::details::allocator a;
    yami::details::mutex mtx;
    yami::details::channel ch;

    mtx.init();

    yami::details::options default_options;
    default_options.init(NULL);
    yami::core::result res = ch.init(a, mtx, default_options, NULL,
        "file://output.bin", NULL, NULL, NULL, NULL, NULL, NULL);
    assert(res == yami::core::ok);

    bool first_frame;

    const char str0[] = {'H', 'e', 'l', 'l', 'o'};
    const char str1[] = {' '};
    const char str2[] = {'c', 'h', 'a', 'n', 'n', 'e', 'l', '\n'};

    char * buffers[3];
    std::size_t buffer_sizes[3];
    buffers[0] = static_cast<char *>(a.allocate(sizeof(str0)));
    std::memcpy(buffers[0], str0, sizeof(str0));
    buffers[1] = static_cast<char *>(a.allocate(sizeof(str1)));
    std::memcpy(buffers[1], str1, sizeof(str1));
    buffers[2] = static_cast<char *>(a.allocate(sizeof(str2)));
    std::memcpy(buffers[2], str2, sizeof(str2));
    buffer_sizes[0] = sizeof(str0);
    buffer_sizes[1] = sizeof(str1);
    buffer_sizes[2] = sizeof(str2);
    res = ch.post(0, buffers, buffer_sizes, 3, first_frame, NULL, NULL);
    assert(res == yami::core::ok);
    assert(first_frame);
    
    assert(ch.get_first_outgoing_frame()->size == sizeof(str0));
    bool close_me = false;
    res = ch.do_some_work(yami::details::output, close_me);
    assert(res == yami::core::ok);
    assert(close_me == false);

    assert(ch.get_first_outgoing_frame()->size == sizeof(str1));
    res = ch.do_some_work(yami::details::output, close_me);
    assert(res == yami::core::ok);
    assert(close_me == false);

    assert(ch.get_first_outgoing_frame()->size == sizeof(str2));
    res = ch.do_some_work(yami::details::output, close_me);
    assert(res == yami::core::ok);

    // this was the last frame, but there is no reason to close the channel
    assert(close_me == false);

    ch.clean();

    std::ifstream file("output.bin");
    std::string line;
    std::getline(file, line);
    assert(file.good());
    assert(line == "Hello channel");
}

namespace // unnamed
{
// used by test4 to test the message dispatch callback

bool dispatch_called = false;
std::vector<std::vector<char> > stored_header_buffers;
std::vector<std::vector<char> > stored_body_buffers;

void incoming_message_dispatch_function(
    void * /* hint */,
    const char * /* source */,
    const char * header_buffers[],
    std::size_t header_buffer_sizes[],
    std::size_t num_of_header_buffers,
    const char * body_buffers[],
    std::size_t body_buffer_sizes[],
    std::size_t num_of_body_buffers)
{
    stored_header_buffers.clear();
    stored_body_buffers.clear();

    for (std::size_t i = 0; i != num_of_header_buffers; ++i)
    {
        std::vector<char> hb;
        hb.assign(header_buffers[i],
            header_buffers[i] + header_buffer_sizes[i]);

        stored_header_buffers.push_back(hb);
    }

    for (std::size_t i = 0; i != num_of_body_buffers; ++i)
    {
        std::vector<char> bb;
        bb.assign(body_buffers[i],
            body_buffers[i] + body_buffer_sizes[i]);

        stored_body_buffers.push_back(bb);
    }

    dispatch_called = true;
}

} // unnamed namespace

// file input - single-frame readout
void test4()
{
    // prepare the file for reading
    char content[] =
    {
        1, 0, 0, 0,             // message id
        0xff, 0xff, 0xff, 0xff, // frame number: -1, the only frame
        4, 0, 0, 0,             // size of message header
        8, 0, 0, 0,             // size of payload
        0, 0, 0, 0,             // empty header
        0, 0, 0, 0              // empty body
    };

    std::ofstream file("input.bin");
    file.write(content, sizeof(content));
    file.close();

    yami::details::allocator a;
    yami::details::mutex mtx;
    yami::details::channel ch;

    mtx.init();

    yami::details::options default_options;
    default_options.init(NULL);
    yami::core::result res = ch.init(a, mtx, default_options, NULL,
        "file://input.bin?read",
        incoming_message_dispatch_function, NULL, NULL, NULL, NULL, NULL);
    assert(res == yami::core::ok);

    // input framelists should be empty
    yami::details::incoming_message_frame_list * first =
        ch.get_first_incoming_frame_list();
    yami::details::incoming_message_frame_list * last =
        ch.get_last_incoming_frame_list();

    assert(first == NULL);
    assert(last == NULL);

    // read the frame header

    assert(ch.get_incoming_state() == yami::details::read_frame_header);
    bool close_me = false;
    res = ch.do_some_work(yami::details::input, close_me);
    assert(res == yami::core::ok);
    assert(close_me == false);

    // the dispatch function should not have been called,
    // it was only the frame header which was read so far
    
    assert(dispatch_called == false);

    // read the payload

    assert(ch.get_incoming_state() == yami::details::read_frame_payload);
    mtx.lock();
    res = ch.do_some_work(yami::details::input, close_me);
    mtx.unlock();
    assert(res == yami::core::ok);
    assert(close_me == false);

    // message is complete

    assert(dispatch_called);

    // message header is empty:
    assert(stored_header_buffers.size() == 1);
    std::vector<char> buf = stored_header_buffers[0];
    assert(buf.size() == 4);
    assert(buf[0] == 0);
    assert(buf[1] == 0);
    assert(buf[2] == 0);
    assert(buf[3] == 0);

    // message body is also empty:
    assert(stored_body_buffers.size() == 1);
    buf = stored_body_buffers[0];
    assert(buf.size() == 4);
    assert(buf[0] == 0);
    assert(buf[1] == 0);
    assert(buf[2] == 0);
    assert(buf[3] == 0);

    ch.clean();
    mtx.clean();
}

// common part for tests 5, 6 and 7
void test_5_6_7(const char * content, std::size_t content_size)
{
    std::ofstream file("input.bin");
    file.write(content, content_size);
    file.close();

    yami::details::allocator a;
    yami::details::mutex mtx;
    yami::details::channel ch;

    mtx.init();

    yami::details::options default_options;
    default_options.init(NULL);
    yami::core::result res = ch.init(a, mtx, default_options, NULL,
        "file://input.bin?read",
        incoming_message_dispatch_function, NULL, NULL, NULL, NULL, NULL);
    assert(res == yami::core::ok);

    // input framelists should be empty
    yami::details::incoming_message_frame_list * first =
        ch.get_first_incoming_frame_list();
    yami::details::incoming_message_frame_list * last =
        ch.get_last_incoming_frame_list();

    assert(first == NULL);
    assert(last == NULL);

    dispatch_called = false;

    // read the first frame header

    assert(ch.get_incoming_state() == yami::details::read_frame_header);
    bool close_me = false;
    res = ch.do_some_work(yami::details::input, close_me);
    assert(res == yami::core::ok);
    assert(close_me == false);

    // the dispatch function should not have been called,
    // it was only the first frame header which was read so far

    assert(dispatch_called == false);

    // read the payload from first frame

    assert(ch.get_incoming_state() == yami::details::read_frame_payload);
    res = ch.do_some_work(yami::details::input, close_me);
    assert(res == yami::core::ok);
    assert(close_me == false);

    // still nothing to dispatch

    assert(dispatch_called == false);

    // read the second frame header

    assert(ch.get_incoming_state() == yami::details::read_frame_header);
    res = ch.do_some_work(yami::details::input, close_me);
    assert(res == yami::core::ok);
    assert(close_me == false);

    // still nothing to dispatch

    assert(dispatch_called == false);

    // read the payload from second frame

    assert(ch.get_incoming_state() == yami::details::read_frame_payload);
    mtx.lock();
    res = ch.do_some_work(yami::details::input, close_me);
    mtx.unlock();
    assert(res == yami::core::ok);
    assert(close_me == false);

    // message is complete

    assert(dispatch_called);

    ch.clean();
    mtx.clean();
}

// file input - multple-frame, single message readout
// header-body border in the middle of frame
void test5()
{
    // prepare the file for reading
    char content[] =
    {
        // first frame:
        1, 0, 0, 0,             // message id
        1, 0, 0, 0,             // frame number: 1
        8, 0, 0, 0,             // size of message header
        4, 0, 0, 0,             // size of payload
        1, 2, 3, 4,             // dummy header

        // second frame:
        1, 0, 0, 0,             // message id
        0xfe, 0xff, 0xff, 0xff, // frame number: -2, the last one
        123, 0, 0, 0,           // size of message header (ignored)
        12, 0, 0, 0,            // size of payload
        5, 6, 7, 8,             // continuation of dummy header
        9, 8, 7, 6,             // dummy body
        5, 4, 3, 2
    };

    test_5_6_7(content, sizeof(content));

    // check dummy message header:
    assert(stored_header_buffers.size() == 2);
    std::vector<char> buf = stored_header_buffers[0];
    assert(buf.size() == 4);
    assert(buf[0] == 1);
    assert(buf[1] == 2);
    assert(buf[2] == 3);
    assert(buf[3] == 4);
    buf = stored_header_buffers[1];
    assert(buf.size() == 4);
    assert(buf[0] == 5);
    assert(buf[1] == 6);
    assert(buf[2] == 7);
    assert(buf[3] == 8);

    // message body is dummy as well
    assert(stored_body_buffers.size() == 1);
    buf = stored_body_buffers[0];
    assert(buf.size() == 8);
    assert(buf[0] == 9);
    assert(buf[1] == 8);
    assert(buf[2] == 7);
    assert(buf[3] == 6);
    assert(buf[4] == 5);
    assert(buf[5] == 4);
    assert(buf[6] == 3);
    assert(buf[7] == 2);
}

// file input - multple-frame, single message readout
// header-body border between frames
void test6()
{
    // prepare the file for reading
    char content[] =
    {
        // first frame:
        1, 0, 0, 0,             // message id
        1, 0, 0, 0,             // frame number: 1
        8, 0, 0, 0,             // size of message header
        8, 0, 0, 0,             // size of payload
        1, 2, 3, 4,             // dummy header
        5, 6, 7, 8,

        // second frame:
        1, 0, 0, 0,             // message id
        0xfe, 0xff, 0xff, 0xff, // frame number: -2, the last one
        123, 0, 0, 0,           // size of message header (ignored)
        8, 0, 0, 0,             // size of payload
        9, 8, 7, 6,             // dummy body
        5, 4, 3, 2
    };

    test_5_6_7(content, sizeof(content));

    // check dummy message header:
    assert(stored_header_buffers.size() == 1);
    std::vector<char> buf = stored_header_buffers[0];
    assert(buf.size() == 8);
    assert(buf[0] == 1);
    assert(buf[1] == 2);
    assert(buf[2] == 3);
    assert(buf[3] == 4);
    assert(buf[4] == 5);
    assert(buf[5] == 6);
    assert(buf[6] == 7);
    assert(buf[7] == 8);

    // message body is dummy as well
    assert(stored_body_buffers.size() == 1);
    buf = stored_body_buffers[0];
    assert(buf.size() == 8);
    assert(buf[0] == 9);
    assert(buf[1] == 8);
    assert(buf[2] == 7);
    assert(buf[3] == 6);
    assert(buf[4] == 5);
    assert(buf[5] == 4);
    assert(buf[6] == 3);
    assert(buf[7] == 2);
}

// file input - multple-frame, single message readout
// header-body border in the middle of frame
void test7()
{
    // prepare the file for reading
    char content[] =
    {
        // first frame:
        1, 0, 0, 0,             // message id
        1, 0, 0, 0,             // frame number: 1
        8, 0, 0, 0,             // size of message header
        12, 0, 0, 0,            // size of payload
        1, 2, 3, 4,             // dummy header
        5, 6, 7, 8,
        9, 8, 7, 6,             // dummy body

        // second frame:
        1, 0, 0, 0,             // message id
        0xfe, 0xff, 0xff, 0xff, // frame number: -2, the last one
        123, 0, 0, 0,           // size of message header (ignored)
        4, 0, 0, 0,             // size of payload
        5, 4, 3, 2              // continuation of dummy body
    };

    test_5_6_7(content, sizeof(content));

    // check dummy message header:
    assert(stored_header_buffers.size() == 1);
    std::vector<char> buf = stored_header_buffers[0];
    assert(buf.size() == 8);
    assert(buf[0] == 1);
    assert(buf[1] == 2);
    assert(buf[2] == 3);
    assert(buf[3] == 4);
    assert(buf[4] == 5);
    assert(buf[5] == 6);
    assert(buf[6] == 7);
    assert(buf[7] == 8);

    // message body is dummy as well
    assert(stored_body_buffers.size() == 2);
    buf = stored_body_buffers[0];
    assert(buf.size() == 4);
    assert(buf[0] == 9);
    assert(buf[1] == 8);
    assert(buf[2] == 7);
    assert(buf[3] == 6);
    buf = stored_body_buffers[1];
    assert(buf.size() == 4);
    assert(buf[0] == 5);
    assert(buf[1] == 4);
    assert(buf[2] == 3);
    assert(buf[3] == 2);
}

// helper for test8
void do_test8(const std::vector<std::pair<int, std::vector<char> > > & frames)
{
    // write all frames to file

    std::ofstream file("input.bin");
    for (std::size_t i = 0; i != frames.size(); ++i)
    {
        const std::vector<char> & fr = frames[i].second;
        file.write(&fr[0], fr.size());
    }
    file.close();

    yami::details::allocator a;
    yami::details::mutex mtx;
    yami::details::channel ch;

    mtx.init();

    yami::details::options default_options;
    default_options.init(NULL);
    yami::core::result res = ch.init(a, mtx, default_options, NULL,
        "file://input.bin?read",
        incoming_message_dispatch_function, NULL, NULL, NULL, NULL, NULL);
    assert(res == yami::core::ok);

    dispatch_called = false;

    // read all frames from file

    for (std::size_t i = 0; i != frames.size(); ++i)
    {
        // read the i frame header

        assert(ch.get_incoming_state() == yami::details::read_frame_header);
        bool close_me = false;
        res = ch.do_some_work(yami::details::input, close_me);
        assert(res == yami::core::ok);
        assert(close_me == false);

        // read the payload from i frame

        assert(ch.get_incoming_state() == yami::details::read_frame_payload);
        mtx.lock();
        res = ch.do_some_work(yami::details::input, close_me);
        mtx.unlock();
        assert(res == yami::core::ok);
        assert(close_me == false);
    }

    // message is complete

    assert(dispatch_called);

    ch.clean();
    mtx.clean();

    // check message header
    assert(stored_header_buffers.size() == 2);
    std::vector<char> buf = stored_header_buffers[0];
    assert(buf.size() == 4);
    assert(buf[0] == 1);
    assert(buf[1] == 2);
    assert(buf[2] == 3);
    assert(buf[3] == 4);
    buf = stored_header_buffers[1];
    assert(buf.size() == 4);
    assert(buf[0] == 5);
    assert(buf[1] == 6);
    assert(buf[2] == 7);
    assert(buf[3] == 8);

    // check message body
    assert(stored_body_buffers.size() == 2);
    buf = stored_body_buffers[0];
    assert(buf.size() == 4);
    assert(buf[0] == 9);
    assert(buf[1] == 8);
    assert(buf[2] == 7);
    assert(buf[3] == 6);
    buf = stored_body_buffers[1];
    assert(buf.size() == 4);
    assert(buf[0] == 5);
    assert(buf[1] == 4);
    assert(buf[2] == 3);
    assert(buf[3] == 2);
}

// helper for test8, frame comparator
bool test8_comp(
    const std::pair<int, std::vector<char> > & left,
    std::pair<int, std::vector<char> > & right)
{
    return left.first < right.first;
}

// all permutations of several frames
void test8()
{
    char frame1[] =
    {
        1, 0, 0, 0,             // message id
        1, 0, 0, 0,             // frame number
        8, 0, 0, 0,             // size of message header
        4, 0, 0, 0,             // size of payload
        1, 2, 3, 4              // dummy header
    };
    char frame2[] =
    {
        1, 0, 0, 0,             // message id
        2, 0, 0, 0,             // frame number
        123, 0, 0, 0,           // size of message header (ignored)
        4, 0, 0, 0,             // size of payload
        5, 6, 7, 8              // continuation of dummy header
    };
    char frame3[] =
    {
        1, 0, 0, 0,             // message id
        3, 0, 0, 0,             // frame number
        123, 0, 0, 0,           // size of message header (ignored)
        4, 0, 0, 0,             // size of payload
        9, 8, 7, 6              // dummy body
    };
    char frame4[] =
    {
        1, 0, 0, 0,             // message id
        0xfc, 0xff, 0xff, 0xff, // frame number (-4: last frame)
        123, 0, 0, 0,           // size of message header (ignored)
        4, 0, 0, 0,             // size of payload
        5, 4, 3, 2              // continuation of dummy body
    };

    std::vector<std::pair<int, std::vector<char> > > frames;
    frames.push_back(std::make_pair(1,
            std::vector<char>(&frame1[0], &frame1[0] + sizeof(frame1))));
    frames.push_back(std::make_pair(2,
            std::vector<char>(&frame2[0], &frame2[0] + sizeof(frame2))));
    frames.push_back(std::make_pair(3,
            std::vector<char>(&frame3[0], &frame3[0] + sizeof(frame3))));
    frames.push_back(std::make_pair(4,
            std::vector<char>(&frame4[0], &frame4[0] + sizeof(frame4))));

    do_test8(frames);
    while (std::next_permutation(frames.begin(), frames.end(), test8_comp))
    {
        do_test8(frames);
    }
}

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
}
