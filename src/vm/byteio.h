//
// byteio.h
//

#ifndef BCDECODE_H
#define BCDECODE_H

#include "common.h"

#include <cstdio>
#include <cstdint>

#include "vm.h"
#include "object/imstring.h"

namespace rhein {

class BinaryReader {
public:
    static bool readByte(FILE* fp, unsigned char& result);
    static bool read32Bit(FILE* fp, uint32_t& result);
    static bool readString(FILE* fp, State* state, String*& result);
    static bool readBER(FILE* fp, unsigned long& result);
    static bool readInt(FILE* fp, long& result);
};

}

#endif // BCDECODE_H

