//
// systable.h
//

#ifndef SYSTABLE_H
#define SYSTABLE_H

#include "internal.h"
#include "object.h"

template <typename K, typename V>
class SysTable : public PlacementNewObj {
public:
    static SysTable* create(State* R) {
        return create(R, kDefaultInitialSize);
    }

    static SysTable* create(State* R, unsigned initial_size) {
        return new (R->allocate_struct<decltype(*this)>) SysTable(R, initial_size);
    }

    unsigned get_num_entries() const { return num_entries_; }

    V find(const K& key) const;

    void insert(State* R, const K& key, const V& value);
    void insert_if_absent(State* R, const K& key, const V& value);

    void remove(State* R, const K& key);
    void remove_if_exists(State* R, const K& key);

    void import(State* R, const SysTable* other);
private:
    const unsigned kDefaultInitialSize = 16;

    SysTable(State* R, unsigned initial_size)
        : table_size_(initial_size) {
    }
    enum class EntryStatus {
        Empty,
        Deleted,
        Exist,
    };

    struct SysTableEntry {
        K key;
        V value;
        EntryStatus status;
    };

    void rehash(State* R);

    unsigned num_entries_;
    unsigned table_size_;
    SysTableEntry* table_;
}

#endif // SYSTABLE_H

