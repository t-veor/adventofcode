#if 0
tmpfile=$(mktemp)
cc "$0" -o "$tmpfile" && "$tmpfile" "$@"
exit
#endif

#include <errno.h>
#include <stdio.h>
#include <inttypes.h>

#include "../c_utils/vec.h"

typedef vec_t(int32_t) vec_int32_t;

void read_input(char* filename, int32_t* start, int32_t* end) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error opening file %s: %d\n", filename, errno);
        exit(1);
    }

    if (fscanf(file, "%" PRId32 "-%" PRId32, start, end) < 2) {
        printf("Error: file not in format {integer}-{integer}");
    }
}

void reverse(vec_int32_t* vec) {
    for (size_t i = 0; i < vec->length / 2; i++) {
        int32_t tmp = vec->data[i];
        vec->data[i] = vec->data[vec->length - i - 1];
        vec->data[vec->length - i - 1] = tmp;
    }
}

void explode_integer(int32_t x, vec_int32_t* out) {
    vec_clear(out);
    while (x) {
        vec_push(out, x % 10);
        x /= 10;
    }
    reverse(out);
}

void digit_freqs(vec_int32_t* integer, int32_t* freqs) {
    for (size_t i = 0; i < 10; i++) {
        freqs[i] = 0;
    }

    for (size_t i = 0; i < integer->length; i++) {
        freqs[integer->data[i]]++;
    }
}

int monotonic(vec_int32_t* vec) {
    for (int i = 0; i < vec->length - 1; i++) {
        if (vec->data[i] > vec->data[i + 1]) {
            return 0;
        }
    }
    return 1;
}

void star1(int32_t start, int32_t end) {
    int32_t freqs[10];
    vec_int32_t digits;
    vec_init(&digits);

    size_t count = 0;

    for (int32_t i = start; i <= end; i++) {
        explode_integer(i, &digits);

        if (!monotonic(&digits)) {
            continue;
        }

        digit_freqs(&digits, freqs);

        int has_two = 0;
        for (size_t i = 0; i < 10; i++) {
            if (freqs[i] >= 2) {
                has_two = 1;
                break;
            }
        }

        if (!has_two) {
            continue;
        }

        count++;
    }

    printf("%zu\n", count);

    vec_done(&digits);
}

void star2(int32_t start, int32_t end) {
    int32_t freqs[10];
    vec_int32_t digits;
    vec_init(&digits);

    size_t count = 0;

    for (int32_t i = start; i <= end; i++) {
        explode_integer(i, &digits);

        if (!monotonic(&digits)) {
            continue;
        }

        digit_freqs(&digits, freqs);

        int has_two = 0;
        for (size_t i = 0; i < 10; i++) {
            if (freqs[i] == 2) {
                has_two = 1;
                break;
            }
        }

        if (!has_two) {
            continue;
        }

        count++;
    }

    printf("%zu\n", count);

    vec_done(&digits);
}

int main(int argc, char** argv) {
    char* filename = "input.txt";
    if (argc > 1) {
        filename = argv[1];
    }

    int32_t start;
    int32_t end;

    read_input(filename, &start, &end);

    star1(start, end);
    star2(start, end);
}
