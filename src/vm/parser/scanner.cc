//
// scanner.cc
//

#include <cctype>
#include <cassert>
#include <cstring>

#include <string>

#include "parser/scanner.h"
#include "basic/port.h"

namespace rhein {

bool
Scanner::fill(size_t additional_required_size) {
    if (p_->eof() && re2c_limit <= re2c_cursor) { return false; }

    ptrdiff_t buffer_rest_size = re2c_limit - re2c_cursor;
    ptrdiff_t marker_index = re2c_marker - re2c_cursor;
    ptrdiff_t ctx_marker_index = re2c_ctx_marker - re2c_cursor;

    bool should_free_old = false;
    char* newbuffer;
    char* newlimit;

    if (additional_required_size > re2c_buffer_size - buffer_rest_size) {
        size_t newsize = 1;
        while (newsize < additional_required_size + buffer_rest_size) {
            newsize *= 2;
        }
        newbuffer = new char[newsize];
        newlimit = newbuffer + newsize;
        re2c_buffer_size = newsize;
        should_free_old = true;
    } else {
        newbuffer = re2c_buffer;
        newlimit = re2c_limit + re2c_buffer_size;
    }

    memmove(newbuffer, re2c_cursor, buffer_rest_size);
    if (should_free_old) { delete[] re2c_buffer; }

    char* dest = newbuffer + buffer_rest_size;
    size_t read_size = p_->read_bytes_as_possible_to_buffer(dest, newlimit - dest);

    re2c_cursor = re2c_buffer = newbuffer;
    re2c_limit = dest + read_size;
    re2c_ctx_marker = newbuffer + ctx_marker_index;
    re2c_marker = newbuffer + marker_index;

    return true;
}

}

