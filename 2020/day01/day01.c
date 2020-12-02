#if 0
tmpfile=$(mktemp)
cc "$0" -o "$tmpfile" && "$tmpfile" "$@"
exit
#endif

#include <errno.h>
#include <inttypes.h>
#include <stdio.h>

#include "../../c_utils/vec.h"

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

void star1(int_vec_t* input) {
    for (size_t i = 0; i < input->length; i++) {
        for (size_t j = i + 1; j < input->length; j++) {
            if (input->data[i] + input->data[j] == 2020) {
                printf("%" PRId32 "\n", input->data[i] * input->data[j]);
                return;
            }
        }
    }
}

void star2(int_vec_t* input) {
    for (size_t i = 0; i < input->length; i++) {
        for (size_t j = i + 1; j < input->length; j++) {
            for (size_t k = j + 1; k < input->length; k++) {
                if (input->data[i] + input->data[j] + input->data[k] == 2020) {
                    printf("%" PRId32 "\n",
                           input->data[i] * input->data[j] * input->data[k]);
                    return;
                }
            }
        }
    }
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
