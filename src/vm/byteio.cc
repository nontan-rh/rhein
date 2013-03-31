//
// byteio.cc
//

#include "object/object.h"
#include "object/imstring.h"
#include "object/function.h"
#include "byteio.h"

namespace rhein {

bool
BinaryReader::readByte(FILE* fp, unsigned char& byte) {
    if (feof(fp)) {
        return false;
    }
    byte = fgetc(fp);
    return true;
}

bool
BinaryReader::read32Bit(FILE* fp, uint32_t& word) {
    uint32_t value = 0;
    for (int i = 0; i < 4; i++) {
        if (feof(fp)) {
            return false;
        }
        value = (value << 8) | fgetc(fp);
    }
    word = value;
    return true;
}

bool
BinaryReader::readString(FILE* fp, State* state, String*& result) {
    unsigned long length;
    if(!BinaryReader::readBER(fp, length)) {
        result = nullptr;
        return false;
    }
    auto buffer = new char[length];
    for(unsigned long i = 0; i < length; i++) {
        if (feof(fp)) {
            delete[] buffer;
            result = nullptr;
            return false;
        }
        buffer[i] = (char)fgetc(fp);
    }
    result = state->s_prv->getString(buffer, length);
    delete[] buffer;
    return true;
}

bool
BinaryReader::readBER(FILE* fp, unsigned long& result) {
    unsigned long value = 0;
    while (true) {
        if (feof(fp)) {
            result = 0;
            return false;
        }
        auto ch = fgetc(fp);
        value = value << 7 | (ch & 0x7f);
        if (!(ch & 0x80)) {
            break;
        }
    }
    result = value;
    return true;
}

bool
BinaryReader::readInt(FILE* fp, long& result) {
    unsigned long zigzag;
    if (!BinaryReader::readBER(fp, zigzag)) {
        result = 0;
        return false;
    }
    long value = 0;
    if (zigzag & 1) { // negative
        value = -(long)(zigzag >> 1) - 1;
    } else { // positive or 0
        value = (long)(zigzag >> 1);
    }
    result = value;
    return true;
}

bool
State::readKlass(FILE* fp) {
    String* klass_name;
    String* parent_name;
    unsigned long slot_num;

    BinaryReader::readString(fp, this, klass_name);
    BinaryReader::readString(fp, this, parent_name);
    BinaryReader::readBER(fp, slot_num);

    auto slots = new String*[slot_num];
    for (unsigned long i = 0; i < slot_num; i++) {
        BinaryReader::readString(fp, this, slots[i]);
    }

    Value parent;
    if (!getKlass(parent_name, parent)) {
        return false;
    }

    addKlass(Klass::create(this, klass_name, get_obj<Klass>(parent), slot_num, slots));
    return true;
}

bool
State::readFunction(FILE* fp) {
    String* function_name;
    unsigned char variable_arg;
    unsigned long argument_num;
    Klass** argument_types;
    unsigned long function_slot_num;
    unsigned long variable_slot_num;
    unsigned long stack_size;
    BinaryReader::readString(fp, this, function_name);
    //function_name->dump();
    BinaryReader::readByte(fp, variable_arg);
    BinaryReader::readBER(fp, argument_num);
    argument_types = new Klass*[argument_num];
    for (unsigned long i = 0; i < argument_num; i++) {
        String* type_name;
        Value klass;
        BinaryReader::readString(fp, this, type_name);
        //type_name->dump();
        getKlass(type_name, klass);
        argument_types[i] = get_obj<Klass>(klass);
    }
    BinaryReader::readBER(fp, function_slot_num);
    BinaryReader::readBER(fp, variable_slot_num);
    BinaryReader::readBER(fp, stack_size);
    unsigned long constant_table_size;
    Value* constant_table;
    BinaryReader::readBER(fp, constant_table_size);
    constant_table = new Value[constant_table_size];
    for (unsigned long i = 0; i < constant_table_size; i++) {
        unsigned char type;
        BinaryReader::readByte(fp, type);
        switch (type) {
            case LiteralSigniture::Int:
                {
                    long int_value;
                    BinaryReader::readInt(fp, int_value);
                    constant_table[i] = int2value(int_value);
                }
                break;
            case LiteralSigniture::Char:
                {
                    uint32_t char_value;
                    BinaryReader::read32Bit(fp, char_value);
                    constant_table[i] = char2value((char)char_value);
                }
                break;
            case LiteralSigniture::String:
                {
                    String* string_value;
                    BinaryReader::readString(fp, this, string_value);
                    constant_table[i] = obj2value(string_value);
                }
                break;
            default:
                return false;
        }
    }
    unsigned long bytecode_length;
    uint32_t* bytecode;
    BinaryReader::readBER(fp, bytecode_length);
    bytecode = new uint32_t[bytecode_length];
    for (unsigned long i = 0; i < bytecode_length; i++) {
        BinaryReader::read32Bit(fp, bytecode[i]);
    }
    addFunction(BytecodeFunction::create(
        this,
        function_name,
        (bool)variable_arg,
        argument_num,
        argument_types,
        function_slot_num,
        variable_slot_num,
        stack_size,
        constant_table_size,
        constant_table,
        bytecode_length,
        bytecode));
    return true;
}

bool
State::readVariable(FILE* fp) {
    String* variable_name;
    BinaryReader::readString(fp, this, variable_name);
    addVariable(variable_name);
    return true;
}

bool
State::readObject(FILE* fp) {
    unsigned char byte;
    BinaryReader::readByte(fp, byte);
    switch (byte) {
        case ObjectSigniture::Class:
            readKlass(fp);
            break;
        case ObjectSigniture::Function:
            readFunction(fp);
            break;
        case ObjectSigniture::Variable:
            readVariable(fp);
            break;
        default:
            return false;
    }
    return true;
}

bool
State::loadFile(FILE* fp) {
    unsigned long item_num;
    if (!BinaryReader::readBER(fp, item_num)) { return false; }
    for (unsigned i = 0; i < item_num; i++) {
        readObject(fp);
    }
    //func_slots->dump();
    return true;
}

};

