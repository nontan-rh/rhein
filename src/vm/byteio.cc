//
// byteio.cc
//

#include "object.h"
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
BinaryReader::readString(FILE* fp, State* R, String*& result) {
	unsigned long length;
	if (!BinaryReader::readBER(fp, length)) {
		result = nullptr;
		return false;
	}
	auto buffer = new char[length];
	for (unsigned long i = 0; i < length; i++) {
		if (feof(fp)) {
			delete[] buffer;
			result = nullptr;
			return false;
		}
		buffer[i] = (char)fgetc(fp);
	}
	result = String::create(R, buffer, length);
	delete[] buffer;
	return true;
}

bool
BinaryReader::readSymbol(FILE* fp, State* R, Symbol*& result) {
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
    result = R->s_prv->get_symbol(buffer, length);
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
    Symbol* klass_name;
    Symbol* parent_name;
    unsigned long slot_num;

    BinaryReader::readSymbol(fp, this, klass_name);
    BinaryReader::readSymbol(fp, this, parent_name);
    BinaryReader::readBER(fp, slot_num);

    auto slots = new Symbol*[slot_num];
    for (unsigned long i = 0; i < slot_num; i++) {
        BinaryReader::readSymbol(fp, this, slots[i]);
    }

    Value parent;
    if (!getKlass(parent_name, parent)) {
        return false;
    }

    addKlass(Klass::create(this, klass_name, parent.get_obj<Klass>(),
    		slot_num, slots));
    return true;
}

bool
State::readFunction(FILE* fp) {
    Symbol* function_name;
    unsigned char variable_arg;
    unsigned long argument_num;
    Symbol** argument_type_ids;
    unsigned long function_slot_num;
    unsigned long variable_slot_num;
    unsigned long stack_size;
    BinaryReader::readSymbol(fp, this, function_name);
    BinaryReader::readByte(fp, variable_arg);
    BinaryReader::readBER(fp, argument_num);
    argument_type_ids = new Symbol*[argument_num];
    for (unsigned long i = 0; i < argument_num; i++) {
        BinaryReader::readSymbol(fp, this, argument_type_ids[i]);
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
                    constant_table[i] = Value::by_int(int_value);
                }
                break;
            case LiteralSigniture::Char:
                {
                    uint32_t char_value;
                    BinaryReader::read32Bit(fp, char_value);
                    constant_table[i] = Value::by_char((char)char_value);
                }
                break;
            case LiteralSigniture::Symbol:
                {
                    Symbol* string_value;
                    BinaryReader::readSymbol(fp, this, string_value);
                    constant_table[i] = Value::by_object(string_value);
                }
                break;
            case LiteralSigniture::String:
            	{
            		String* string_value;
            		BinaryReader::readString(fp, this, string_value);
            		constant_table[i] = Value::by_object(string_value);
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
        FunctionInfo::create(this, function_name, (bool)variable_arg, argument_num, argument_type_ids),
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
        default:
            return false;
    }
    return true;
}

bool
State::loadFile(FILE* fp) {
    Symbol* init_name;
    BinaryReader::readSymbol(fp, this, init_name);
    unsigned long item_num;
    if (!BinaryReader::readBER(fp, item_num)) { return false; }
    for (unsigned i = 0; i < item_num; i++) {
        readObject(fp);
    }
    execute(this, init_name, 0, nullptr);
    return true;
}

};

