//
// systable.h
//

#ifndef SYSTABLE_H
#define SYSTABLE_H

#include <cassert>

#include "vm.h"
#include "internal.h"
#include "object.h"

namespace rhein {

static inline unsigned
get_sys_hash(const void* p) {
    return static_cast<unsigned>(reinterpret_cast<uintptr_t>(p));
}

static inline unsigned
get_sys_hash(unsigned i) {
    return i;
}

static inline unsigned
get_sys_hash(int i) {
    return static_cast<unsigned>(i);
}

template <typename K, typename V>
class SysTable : public PlacementNewObj {
public:
    static SysTable* create() {
        return create(kDefaultInitialSize);
    }

    static SysTable* create(unsigned initial_size) {
        return new (get_current_state()->allocate_struct<SysTable<K,V>>()) SysTable(initial_size);
    }

    unsigned get_num_entries() const { return num_entries_; }

    bool exists(const K& key) const {
        unsigned hash_value = get_sys_hash(key);
        unsigned index_begin = hash_value % table_size_;

        if (table_[index_begin].status == EntryStatus::Empty) { return false; }
        if (entry_is(index_begin, key)) { return true; }

        for (unsigned i = (index_begin + 1) % table_size_;
                !(i == index_begin || table_[i].status == EntryStatus::Empty);
                i = (i + 1) % table_size_) {
            if (entry_is(i, key)) { return true; }
        }

        return false;
    }

    V find(const K& key) const {
        unsigned hash_value = get_sys_hash(key);
        unsigned index_begin = hash_value % table_size_;

        if (table_[index_begin].status == EntryStatus::Empty) { assert(false); }

        if (entry_is(index_begin, key)) {
            return table_[index_begin].value;
        }

        for (unsigned i = (index_begin + 1) % table_size_;
                !(i == index_begin || table_[i].status == EntryStatus::Empty);
                i = (i + 1) % table_size_) {
            if (entry_is(i, key)) { return table_[i].value; }
        }

        assert(false);
    }

    void insert(const K& key, const V& value) {
        unsigned hash_value = get_sys_hash(key);
        unsigned index_begin = hash_value % table_size_;

        if (table_[index_begin].status == EntryStatus::Empty) {
            set_entry(index_begin, key, value);
            table_used_++;
            num_entries_++;
            rebuild_if_required();
            return;
        } else if (entry_is(index_begin, key)) {
            table_[index_begin].value = value;
            return;
        }

        for (unsigned i = (index_begin + 1) % table_size_; i != index_begin;
                i = (i + 1) % table_size_) {
            if (table_[i].status == EntryStatus::Empty) {
                set_entry(i, key, value);
                table_used_++;
                num_entries_++;
                rebuild_if_required();
                return;
            } else if (entry_is(i, key)) {
                table_[i].value = value;
                return;
            }
        }

        assert(false);
    }

    void insert_if_absent(const K& key, const V& value) {
        if (exists(key)) { return; }
        else { insert(key, value); }
    }

    void assign(const K& key, const V& value) {
        if (!exists(key)) { return; }
        else { insert(key, value); }
    }

    void remove(const K& key) {
        unsigned hash_value = get_sys_hash(key);
        unsigned index_begin = hash_value % table_size_;

        if (table_[index_begin].status == EntryStatus::Empty) { assert(false); }
        
        if (entry_is(index_begin, key)) {
            table_[index_begin].status = EntryStatus::Deleted;
            num_entries_--;
            return;
        }

        for (unsigned i = (index_begin + 1) % table_size_;
                !(i == index_begin || table_[i].status == EntryStatus::Empty);
                i = (i + 1) % table_size_) {
            if (entry_is(i, key)) {
                table_[i].status = EntryStatus::Deleted;
                num_entries_--;
                return;
            }
        }

        assert(false);
    }

    void import(const SysTable<K,V>* other) {
        for (unsigned i = 0; i < other->table_size_; i++) {
            insert(other->table_[i].key, other->table_[i].value);
        }
    }

private:
    const static unsigned kDefaultInitialSize = 17;
    const double kRebuildRatio = 0.60;

    SysTable(unsigned initial_size)
        : table_size_(initial_size), table_used_(0) {

        table_ = get_current_state()->allocate_block<SysTableEntry>(initial_size);
        for (unsigned i = 0; i < table_size_; i++) {
            table_[i].status = EntryStatus::Empty;
        }
    }

    bool entry_is(unsigned index, const K& key) const {
        return (table_[index].status == EntryStatus::Exist
                && table_[index].key == key);
    }

    void set_entry(unsigned index, const K& key, const V& value) {
        table_[index].status = EntryStatus::Exist;
        table_[index].key = key;
        table_[index].value = value;
    }

    void rebuild_if_required() {
        if (static_cast<double>(table_used_) / table_size_ > kRebuildRatio) {
            rebuild();
        }
    }

    void rebuild() {
        unsigned newtable_size = table_size_ * 2 + 1;
        SysTableEntry* newtable = get_current_state()->allocate_block<SysTableEntry>(newtable_size);

        for (unsigned i = 0; i < newtable_size; i++) {
            newtable[i].status = EntryStatus::Empty;
        }

        for (unsigned i = 0; i < table_size_; i++) {
            if (table_[i].status == EntryStatus::Exist) {
                unsigned hash_value = get_sys_hash(table_[i].key);
                unsigned newindex = hash_value % newtable_size;
                if (newtable[newindex].status == EntryStatus::Empty) {
                    newtable[newindex].status = EntryStatus::Exist;
                    newtable[newindex].key = table_[i].key;
                    newtable[newindex].value = table_[i].value;
                    continue;
                }

                for (unsigned j = (newindex + 1) % newtable_size; ;
                        j = (j + 1) % newtable_size) {
                    assert(newindex != j);
                    if (newtable[j].status == EntryStatus::Empty) {
                        newtable[j].status = EntryStatus::Exist;
                        newtable[j].key = table_[i].key;
                        newtable[j].value = table_[i].value;
                        break;
                    }
                }
            }
        }

        table_used_ = num_entries_;
        table_size_ = newtable_size;
        table_ = newtable;
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

    unsigned num_entries_;
    unsigned table_size_;
    unsigned table_used_;
    SysTableEntry* table_;
};

}

#endif // SYSTABLE_H

