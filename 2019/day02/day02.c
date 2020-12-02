#if 0
tmpfile=$(mktemp)
cc "$0" -o "$tmpfile" && "$tmpfile" "$@"
exit
#endif

#include <errno.h>
#include <stdio.h>

#include "../../c_utils/vec.h"

typedef vec_t(size_t) vec_size_t;

void read_input(char* filename, vec_size_t* vec) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error opening file %s: %d\n", filename, errno);
        exit(1);
    }

    size_t x;
    while (fscanf(file, "%zu, ", &x) != EOF) {
        vec_push(vec, x);
    }

    fclose(file);
}

size_t run_program(const vec_size_t* input, size_t noun, size_t verb) {
    vec_size_t cloned_input;
    vec_clone(input, &cloned_input);
    size_t* memory = cloned_input.data;
    memory[1] = noun;
    memory[2] = verb;

    size_t pc = 0;
    for (;;) {
        size_t opcode = memory[pc];
        if (opcode == 99) {
            break;
        }

        size_t src = memory[pc + 1];
        size_t trg = memory[pc + 2];
        size_t dst = memory[pc + 3];
        size_t op1 = memory[src];
        size_t op2 = memory[trg];

        if (opcode == 1) {
            memory[dst] = op1 + op2;
        } else if (opcode == 2) {
            memory[dst] = op1 * op2;
        } else {
            printf("Unknown opcode %zu\n", opcode);
            exit(1);
        }

        pc += 4;
    }

    size_t result = memory[0];
    vec_done(&cloned_input);

    return result;
}

void star1(const vec_size_t* input) {
    printf("%zu\n", run_program(input, 12, 2));
}

void star2(const vec_size_t* input) {
    const size_t target = 19690720;
    for (size_t noun = 0; noun < 100; noun++) {
        for (size_t verb = 0; verb < 109; verb++) {
            if (run_program(input, noun, verb) == target) {
                printf("%zu\n", 100 * noun + verb);
                return;
            }
        }
    }
}

int main(int argc, char** argv) {
    char* filename = "input.txt";
    if (argc > 1) {
        filename = argv[1];
    }

    vec_size_t input;
    vec_init(&input);
    read_input(filename, &input);

    star1(&input);
    star2(&input);

    vec_done(&input);
}
