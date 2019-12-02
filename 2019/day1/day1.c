#if 0
tmpfile=$(mktemp)
cc "$0" -o "$tmpfile" && "$tmpfile" "$@"
exit
#endif

#include <errno.h>
#include <inttypes.h>
#include <stdio.h>

#include "../c_utils/utils.h"

typedef vec_t(int32_t) int_vec_t;

void read_input(char* filename, int_vec_t* vec) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error opening file %s: %d\n", filename, errno);
        exit(1);
    }

    int32_t x;
    while (fscanf(file, "%" PRId32 " ", &x) != EOF) {
        vec_push(vec, x);
    }

    fclose(file);
}

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
    vec_init(&input);

    read_input(filename, &input);

    star1(&input);
    star2(&input);

    vec_done(&input);
}
