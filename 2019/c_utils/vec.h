#ifndef VEC_H_
#define VEC_H_

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Basic vector implementation
#define VECTOR_INITIAL_CAPACITY 16
#define VECTOR_RESIZE_FACTOR 2

void __vec_init(size_t data_size, size_t* length, size_t* capacity,
                char** data) {
    *capacity = VECTOR_INITIAL_CAPACITY;
    *length = 0;
    *data = (char*)malloc(VECTOR_INITIAL_CAPACITY * data_size);
}

void __vec_expand(size_t data_size, size_t* length, size_t* capacity,
                  char** data) {
    if (*length == *capacity) {
        *capacity *= VECTOR_RESIZE_FACTOR;
        *data = (char*)realloc(*data, *capacity * data_size);
    }
}

#define vec_t(T)                                                               \
    struct {                                                                   \
        size_t length;                                                         \
        size_t capacity;                                                       \
        T* data;                                                               \
    }

#define __vec_explode(v)                                                       \
    sizeof(*(v)->data), &(v)->length, &(v)->capacity, (char**)&(v)->data

#define vec_init(v) __vec_init(__vec_explode(v))

#define vec_push(v, x)                                                         \
    do {                                                                       \
        __vec_expand(__vec_explode(v));                                        \
        (v)->data[(v)->length++] = (x);                                        \
    } while (0)

#define vec_clone(u, v)                                                        \
    do {                                                                       \
        (v)->length = (u)->length;                                             \
        (v)->capacity = (u)->capacity;                                         \
        char** data = (char**)&(v)->data;                                      \
        *data = (char*)malloc((v)->capacity * sizeof(*(v)->data));             \
        for (size_t i = 0; i < (v)->length; i++) {                             \
            (v)->data[i] = (u)->data[i];                                       \
        }                                                                      \
    } while (0)

#define vec_clear(v)                                                           \
    do {                                                                       \
        (v)->length = 0;                                                       \
    } while(0)

#define vec_done(v)                                                            \
    do {                                                                       \
        free((v)->data);                                                       \
        (v)->length = 0;                                                       \
        (v)->capacity = 0;                                                     \
    } while (0)

#endif // VEC_H_
