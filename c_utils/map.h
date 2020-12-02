#ifndef MAP_H_
#define MAP_H_

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Hashmap implementation
#define MAP_INITIAL_CAPACITY 16
#define MAP_MAX_LOAD_FACTOR 0.6

typedef enum {
    __EMPTY = 0,
    __AVAILABLE,
    __FILLED,
} __map_slot_state;

typedef struct {
    __map_slot_state state;
    uint64_t hash;
    char* key;
    char* value;
} __map_slot_t;

typedef struct {
    size_t capacity;
    size_t occupied;
    __map_slot_t* slots;
} __map_base_t;

// djb2 hash algorithm
uint64_t generic_hash(const char* key, size_t key_size) {
    uint64_t hash = 5381;
    for (size_t i = 0; i < key_size; i++) {
        hash = ((hash << 5) + hash) + key[i];
    }
    return hash;
}

void __map_init(__map_base_t* base) {
    base->capacity = MAP_INITIAL_CAPACITY;
    base->occupied = 0;
    base->slots = (__map_slot_t*)malloc(base->capacity * sizeof(__map_slot_t));
    memset(base->slots, 0, base->capacity * sizeof(__map_slot_t));
}

void __map_slot_done(__map_slot_t* slot) {
    slot->state = __EMPTY;
    slot->hash = 0;
    if (slot->key != NULL) {
        free(slot->key);
        slot->key = NULL;
    }
    if (slot->value != NULL) {
        free(slot->value);
        slot->value = NULL;
    }
}

void __map_done(__map_base_t* base) {
    for (size_t i = 0; i < base->capacity; i++) {
        __map_slot_done(&base->slots[i]);
    }
    base->capacity = 0;
    base->occupied = 0;
    free(base->slots);
}

__map_slot_t* __map_get(__map_base_t* base, const char* key, uint64_t hash,
                        int (*eq)(const char*, const char*)) {
    size_t start = hash & (base->capacity - 1);
    size_t index = start;
    do {
        __map_slot_t* candidate = &base->slots[index];
        if (candidate->state == __EMPTY) {
            return NULL;
        }
        if (candidate->state == __FILLED && candidate->hash == hash &&
            eq(key, candidate->key)) {
            return candidate;
        }

        index = (index + 1) & (base->capacity - 1);
    } while (index != start);

    return NULL;
}

void __map_expand(__map_base_t* base) {
    size_t old_capacity = base->capacity;
    __map_slot_t* old_slots = base->slots;
    base->capacity *= 2;
    base->slots = (__map_slot_t*)malloc(base->capacity * sizeof(__map_slot_t));
    memset(base->slots, 0, base->capacity * sizeof(__map_slot_t));

    for (size_t i = 0; i < old_capacity; i++) {
        if (old_slots[i].state == __FILLED) {
            __map_slot_t* old_slot = &old_slots[i];
            // insert without checking equality (we can assume all keys are not
            // equal)
            size_t index = old_slot->hash & (base->capacity - 1);
            for (;;) {
                __map_slot_t* candidate = &base->slots[index];
                if (candidate->state < __FILLED) {
                    *candidate = *old_slot;
                    break;
                }

                index = (index + 1) & (base->capacity - 1);
            }
        }
    }

    free(old_slots);
}

void __map_insert(size_t key_size, size_t value_size, __map_base_t* base,
                  const char* key, const char* value, uint64_t hash,
                  int (*eq)(const char*, const char*)) {
    // first do a check to see if the key is aleady in the map
    __map_slot_t* already_in_map = __map_get(base, key, hash, eq);
    if (already_in_map != NULL) {
        // replace
        memcpy(already_in_map->value, value, value_size);
        return;
    }

    // key is not already in the map, may need to expand
    if ((double)base->occupied / (double)base->capacity > MAP_MAX_LOAD_FACTOR) {
        __map_expand(base);
    }

    size_t index = hash & (base->capacity - 1);
    for (;;) {
        __map_slot_t* candidate = &base->slots[index];
        if (candidate->state < __FILLED) {
            candidate->state = __FILLED;
            candidate->hash = hash;
            candidate->key = (char*)malloc(key_size);
            memcpy(candidate->key, key, key_size);
            candidate->value = (char*)malloc(value_size);
            memcpy(candidate->value, value, value_size);
            break;
        }

        index = (index + 1) & (base->capacity - 1);
    }

    base->occupied++;
}

size_t __map_iter_init(__map_base_t* base, char** key, char** value) {
    if (base->occupied == 0) {
        *key = NULL;
        *value = NULL;
        return base->capacity;
    }

    size_t index = 0;
    while (index < base->capacity && base->slots[index].state == __EMPTY) {
        index++;
    }
    if (index < base->capacity) {
        *key = base->slots[index].key;
        *value = base->slots[index].value;
    }
    return index;
}

size_t __map_iter_next(__map_base_t* base, char** key, char** value,
                       size_t index) {
    do {
        index++;
    } while (index < base->capacity && base->slots[index].state == __EMPTY);
    if (index < base->capacity) {
        *key = base->slots[index].key;
        *value = base->slots[index].value;
    }
    return index;
}

#define map_t(K, V)                                                            \
    struct {                                                                   \
        __map_base_t __base;                                                   \
        uint64_t (*__hasher)(const char*);                                     \
        int (*__eq)(const char*, const char*);                                 \
        K* key;                                                                \
        V* value;                                                              \
    }

#define map_init(m, hasher, eq)                                                \
    do {                                                                       \
        __map_init(&(m)->__base);                                              \
        (m)->__hasher = hasher;                                                \
        (m)->__eq = eq;                                                        \
        (m)->key = NULL;                                                       \
        (m)->value = NULL;                                                     \
    } while (0)

#define map_done(m)                                                            \
    do {                                                                       \
        __map_done(&(m)->__base);                                              \
        (m)->key = NULL;                                                       \
        (m)->value = NULL;                                                     \
    } while (0)

#define map_get(m, k)                                                          \
    do {                                                                       \
        __map_slot_t* slot =                                                   \
            __map_get(&(m)->__base, (const char*)&(k),                         \
                      (m)->__hasher((const char*)&(k)), (m)->__eq);            \
        if (slot == NULL) {                                                    \
            (m)->key = NULL;                                                   \
            (m)->value = NULL;                                                 \
        } else {                                                               \
            char** key_ptr = (char**)(&(m)->key);                              \
            char** value_ptr = (char**)(&(m)->value);                          \
            *key_ptr = slot->key;                                              \
            *value_ptr = slot->value;                                          \
        }                                                                      \
    } while (0)

#define map_insert(m, k, v)                                                    \
    __map_insert(sizeof(*(m)->key), sizeof(*(m)->value), &(m)->__base,         \
                 (const char*)&(k), (const char*)&(v),                         \
                 (m)->__hasher((const char*)&(k)), (m)->__eq)

#define map_foreach(m)                                                         \
    for (size_t __i = __map_iter_init(&(m)->__base, (char**)&(m)->key,         \
                                      (char**)&(m)->value);                    \
         __i < (m)->__base.capacity;                                           \
         __i = __map_iter_next(&(m)->__base, (char**)&(m)->key,                \
                               (char**)&(m)->value, __i))

#endif // MAP_H_
