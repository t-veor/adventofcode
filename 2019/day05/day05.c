#if 0
tmpfile=$(mktemp)
cc "$0" -o "$tmpfile" && "$tmpfile" "$@"
exit
#endif

#include <errno.h>
#include <stdio.h>
#include <inttypes.h>

#define bool int
#define true 1
#define false 0

#include "../c_utils/vec.h"

typedef vec_t(int32_t) vec_int32_t;

typedef struct {
    vec_int32_t memory;
    size_t pc;
    bool halted;
} int_code_vm_t;

typedef enum {
    VM_HALTED,
    VM_READ_INPUT,
    VM_HAS_OUTPUT,
    VM_CONTINUE,
} vm_status_code;

typedef struct {
    int32_t opcode;
    size_t src, trg, dst;
    int32_t op1, op2, op3;
} decode_out_t;

void vm_init(int_code_vm_t* vm, vec_int32_t* memory) {
    vec_clone(memory, &vm->memory);
    vm->pc = 0;
    vm->halted = false;
}

void vm_done(int_code_vm_t* vm) {
    vec_done(&vm->memory);
}

int32_t vm_read(int_code_vm_t* vm, size_t loc) {
    if (loc < vm->memory.length) {
        return vm->memory.data[loc];
    }
    return 0;
}

void vm_write(int_code_vm_t* vm, size_t loc, int32_t x) {
    vm->memory.data[loc] = x;
}

int32_t vm_get_with_mode(int_code_vm_t* vm, int32_t mode, int32_t source) {
    if (mode == 1) {
        return source;
    } else {
        return vm_read(vm, (size_t)source);
    }
}

decode_out_t vm_fetch_decode(int_code_vm_t* vm) {
    decode_out_t result;

    int32_t op = vm_read(vm, vm->pc);
    result.opcode = op % 100;
    int32_t src_mode = (op % 1000) / 100;
    int32_t trg_mode = (op % 10000) / 1000;
    int32_t dst_mode = (op % 100000) / 10000;

    int32_t src = vm_read(vm, vm->pc + 1);
    int32_t trg = vm_read(vm, vm->pc + 2);
    int32_t dst = vm_read(vm, vm->pc + 3);

    result.src = (size_t)src;
    result.trg = (size_t)trg;
    result.dst = (size_t)dst;

    result.op1 = vm_get_with_mode(vm, src_mode, src);
    result.op2 = vm_get_with_mode(vm, trg_mode, trg);
    result.op3 = vm_get_with_mode(vm, dst_mode, dst);

    return result;
}

vm_status_code vm_step(int_code_vm_t* vm, int32_t input, int32_t* output) {
    if (vm->halted) {
        return VM_HALTED;
    }

    decode_out_t decode = vm_fetch_decode(vm);
    switch (decode.opcode) {
        case 99:
            vm->halted = true;
            return VM_HALTED;
        case 1:
            vm_write(vm, decode.dst, decode.op1 + decode.op2);
            vm->pc += 4;
            return VM_CONTINUE;
        case 2:
            vm_write(vm, decode.dst, decode.op1 * decode.op2);
            vm->pc += 4;
            return VM_CONTINUE;
        case 3:
            vm_write(vm, decode.src, input);
            vm->pc += 2;
            return VM_READ_INPUT;
        case 4:
            *output = decode.op1;
            vm->pc += 2;
            return VM_HAS_OUTPUT;
        case 5:
            vm->pc = decode.op1 != 0 ? (size_t)decode.op2 : vm->pc + 3;
            return VM_CONTINUE;
        case 6:
            vm->pc = decode.op1 == 0 ? (size_t)decode.op2 : vm->pc + 3;
            return VM_CONTINUE;
        case 7:
            vm_write(vm, decode.dst, decode.op1 < decode.op2);
            vm->pc += 4;
            return VM_CONTINUE;
        case 8:
            vm_write(vm, decode.dst, decode.op1 == decode.op2);
            vm->pc += 4;
            return VM_CONTINUE;
        default:
            printf("Unknown opcode %" PRId32 "\n", decode.opcode);
            exit(1);
            return VM_CONTINUE;
    }
}

void vm_run_until_complete(int_code_vm_t* vm, vec_int32_t* inputs,
                           vec_int32_t* outputs) {
    int32_t next_input;
    size_t input_index = 0;
    int32_t output;

    for (;;) {
        if (input_index < inputs->length) {
            next_input = inputs->data[input_index];
        } else {
            next_input = 0;
        }

        switch (vm_step(vm, next_input, &output)) {
            case VM_READ_INPUT:
                input_index++;
                break;
            case VM_HAS_OUTPUT:
                vec_push(outputs, output);
                break;
            case VM_CONTINUE:
                break;
            case VM_HALTED:
            default:
                return;
        }
    }
}

void read_input(char* filename, vec_int32_t* vec) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error opening file %s: %d\n", filename, errno);
        exit(1);
    }

    int32_t x;
    while (fscanf(file, "%" PRId32 ", ", &x) != EOF) {
        vec_push(vec, x);
    }

    fclose(file);
}

void star1(vec_int32_t* program) {
    int_code_vm_t vm;
    vm_init(&vm, program);

    vec_int32_t inputs;
    vec_init(&inputs);
    vec_push(&inputs, 1);

    vec_int32_t outputs;
    vec_init(&outputs);

    vm_run_until_complete(&vm, &inputs, &outputs);
    printf("%d\n", outputs.data[outputs.length - 1]);

    vec_done(&outputs);
    vec_done(&inputs);
    vm_done(&vm);
}

void star2(vec_int32_t* program) {
    int_code_vm_t vm;
    vm_init(&vm, program);

    vec_int32_t inputs;
    vec_init(&inputs);
    vec_push(&inputs, 5);

    vec_int32_t outputs;
    vec_init(&outputs);

    vm_run_until_complete(&vm, &inputs, &outputs);
    printf("%d\n", outputs.data[0]);

    vec_done(&outputs);
    vec_done(&inputs);
    vm_done(&vm);
}

int main(int argc, char** argv) {
    char* filename = "input.txt";
    if (argc > 1) {
        filename = argv[1];
    }

    vec_int32_t program;
    vec_init(&program);
    read_input(filename, &program);

    star1(&program);
    star2(&program);

    vec_done(&program);
}
