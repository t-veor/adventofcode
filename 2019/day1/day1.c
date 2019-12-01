#if 0
tmpfile=$(mktemp)
cc "$0" -o "$tmpfile" && "$tmpfile" "$@"
exit
#endif

#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>
#include <errno.h>

/// INPUT HANDLING
// Very basic vector implementation
#define INITIAL_CAPACITY 16

typedef struct {
    int32_t* data;
    size_t length;
    size_t capacity;
} int_vec_t;

void int_vec_init(int_vec_t* vec) {
    vec->capacity = INITIAL_CAPACITY;
    vec->length = 0;
    vec->data = malloc(vec->capacity * sizeof(int32_t));
}

void int_vec_push(int_vec_t* vec, int32_t datum) {
    if (vec->length == vec->capacity) {
        vec->capacity *= 2;
        vec->data = realloc(vec->data, vec->capacity * sizeof(int32_t));
    }

    vec->data[vec->length++] = datum;
}

void int_vec_done(int_vec_t* vec) {
    free(vec->data);
    vec->length = 0;
    vec->capacity = 0;
}

void read_input(char* filename, int_vec_t* vec) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error opening file %s: %d\n", filename, errno);
        exit(1);
    }

    int32_t x;
    while (fscanf(file, "%" PRId32 " ", &x) != EOF) {
        int_vec_push(vec, x);
    }
}
/// END INPUT HANDLING

int32_t fuel_reqs(int32_t mass) {
    return mass / 3 - 2;
}

int32_t real_fuel_reqs(int32_t mass) {
    int32_t remaining = mass;
    int32_t total = 0;

    while (1) {
        remaining = remaining / 3 - 2;
        if (remaining < 0) {
            break;
        }
        total += remaining;
    }

    return total;
}

void star1(int_vec_t* input) {
    int32_t total = 0;
    for (size_t i = 0; i < input->length; i++) {
        total += fuel_reqs(input->data[i]);
    }
    printf("%" PRId32 "\n", total);
}

void star2(int_vec_t* input) {
    int32_t total = 0;
    for (size_t i = 0; i < input->length; i++) {
        total += real_fuel_reqs(input->data[i]);
    }
    printf("%" PRId32 "\n", total);
}

int main(int argc, char** argv) {
    char* filename = "input.txt";
    if (argc > 1) {
        filename = argv[1];
    }

    int_vec_t input;
    int_vec_init(&input);

    read_input(filename, &input);

    star1(&input);
    star2(&input);

    int_vec_done(&input);
}
