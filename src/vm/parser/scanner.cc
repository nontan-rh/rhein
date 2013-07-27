//
// scanner.cc
//

#include <iostream>

#include <cctype>
#include <cassert>
#include <cstring>
#include <algorithm>

#include <string>

#include "parser/scanner.h"
#include "basic/port.h"

namespace rhein {

bool
Scanner::fill(size_t additional_required_size) {
    using namespace std;
    if (p_->eof() && re2c_limit <= re2c_cursor) { return false; }

    char* buffer_bottom = re2c_token;
    buffer_bottom = std::min(re2c_ctx_marker, buffer_bottom);
    buffer_bottom = std::min(re2c_marker, buffer_bottom);

    ptrdiff_t buffer_filled_size = re2c_limit      - buffer_bottom;
    ptrdiff_t token_index        = re2c_token      - buffer_bottom;
    ptrdiff_t cursor_index       = re2c_cursor     - buffer_bottom;
    ptrdiff_t marker_index       = re2c_marker     - buffer_bottom;
    ptrdiff_t ctx_marker_index   = re2c_ctx_marker - buffer_bottom;

    bool should_free_old = false;
    char* newbuffer;
    char* newbuffer_end;

    if (additional_required_size > re2c_buffer_size - buffer_filled_size) {
        size_t newsize = 1;
        while (newsize < additional_required_size + buffer_filled_size) {
            newsize *= 2;
        }

        newbuffer = new char[newsize];
        newbuffer_end = newbuffer + newsize;
        re2c_buffer_size = newsize;
        should_free_old = true;
    } else {
        newbuffer = re2c_buffer;
        newbuffer_end = re2c_buffer + re2c_buffer_size;
    }

    memmove(newbuffer, buffer_bottom, buffer_filled_size);
    if (should_free_old) { delete[] re2c_buffer; }

    char* dest = newbuffer + buffer_filled_size;
    size_t read_size = p_->read_bytes_as_possible_to_buffer(dest,
            newbuffer_end - dest);

    re2c_buffer     = newbuffer;
    re2c_limit      = dest + read_size;
    re2c_token      = re2c_buffer + token_index;
    re2c_cursor     = re2c_buffer + cursor_index;
    re2c_marker     = re2c_buffer + marker_index;
    re2c_ctx_marker = re2c_buffer + ctx_marker_index;

    return true;
}

ScannerModule*
ScannerModule::create() {
    void* p = R->allocate_struct<ScannerModule>();
    return new (p) ScannerModule;
}

bool
ScannerModule::initialize() {
    R->add_class("Token", "any");
    return false;
}

}

