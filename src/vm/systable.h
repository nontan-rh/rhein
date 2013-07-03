//
// systable.h
//

#ifndef SYSTABLE_H
#define SYSTABLE_H

#include "internal.h"
#include "object.h"

namespace rhein {

unsigned get_sys_hash(void* p) {
    return static_cast<unsigned>(reinterpret_cast<uintptr_t>(p));
}

unsigned get_sys_hash(unsigned i) {
    return i;
}

unsigned get_sys_hash(int i) {
    return static_cast<unsigned>(i);
}

template <typename K, typename V>
class SysTable : public PlacementNewObj {
public:
    static SysTable* create(State* R) {
        return create(R, kDefaultInitialSize);
    }

    static SysTable* create(State* R, unsigned initial_size) {
        return new (R->allocate_struct<SysTable<K,V>>()) SysTable(R, initial_size);
    }

    unsigned get_num_entries() const { return num_entries_; }

    V find(const K& key) const {
        unsigned hash_value = get_sys_hash(key);
        unsigned index_begin = hash_value % table_size_;

        if (table_[index_begin].status == EntryStatus::Empty) {
            throw "";
        }

        if (table_[index_begin].key == key) {
            return table_[index_begin].value;
        }

        for (int i = (index_begin + 1) % table_size_;
                !(i == index_begin || table_[i].status == EntryStatus::Empty);
                i = (i + 1) % table_size_) {
            if (table_[i].key == key) {
                return table_[i].value;
            }
        }
        throw "";
    }

    void insert(State* R, const K& key, const V& value) {
        unsigned hash_value = get_sys_hash(key);
        unsigned index_begin = hash_value % table_size_;

        if (table_[index_begin].status == EntryStatus::Empty) {
            table_[index_begin].status = EntryStatus::Exist;
            table_[index_begin].key = key;
            table_[index_begin].value = value;
            table_used_++;
            num_entries_++;

            if (static_cast<double>(table_used_)
                    / table_size_ > kRebuildRatio) {
                rebuild(R);
            }
            return;
        } else if (entry_is(index_begin, key)) {
            table_[index_begin].value = value;
            return;
        }

        for (int i = (index_begin + 1) % table_size_; i != index_begin;
                i = (i + 1) % table_size_) {
            if (table_[i].status == EntryStatus::Empty) {
                table_[i].status = EntryStatus::Exist;
                table_[i].key = key;
                table_[i].value = value;
                table_used_++;
                num_entries_++;
                if (static_cast<double>(table_used_)
                        / table_size_ > kRebuildRatio) {
                    rebuild(R);
                }
                return;
            } else if (entry_is(i, key)) {
                table_[i].value = value;
                return;
            }
        }

        throw "";
    }

    void remove(const K& key) {
        unsigned hash_value = get_sys_hash(key);
        unsigned index_begin = hash_value % table_size_;

        if (table_[index_begin].status == EntryStatus::Empty) {
            throw "";
        }
        
        if (entry_is(index_begin, key)) {
            table_[index_begin].status = EntryStatus::Deleted;
            num_entries_--;
            return;
        }

        for (int i = (index_begin + 1) % table_size_;
                !(i == index_begin || table_[i].status == EntryStatus::Empty);
                i = (i + 1) % table_size_) {
            if (entry_is(i, key)) {
                table_[i].status = EntryStatus::Deleted;
                num_entries_--;
                return;
            }
        }

        throw "";
    }

    void import(State* R, const SysTable<K,V>* other) {
        for (unsigned i = 0; i < other->table_size_; i++) {
            insert(R, other->table_[i].key, other->table_[i].value);
        }
    }

private:
    const static unsigned kDefaultInitialSize = 17;
    const double kRebuildRatio = 0.60;

    SysTable(State* R, unsigned initial_size)
        : table_size_(initial_size), table_used_(0) {

        table_ = R->allocate_block<SysTableEntry>(initial_size);
    }

    bool entry_is(unsigned index, const K& key) {
        return (table_[index].status == EntryStatus::Exist
                && table_[index].key == key);
    }

    void rebuild(State* R) {
        unsigned newtable_size = table_size_ * 2 + 1;
        SysTableEntry* newtable = R->allocate_block<SysTableEntry>(newtable_size);

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

                for (int j = (newindex + 1) % table_size_; ;
                        j = (j + 1) % table_size_) {
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

