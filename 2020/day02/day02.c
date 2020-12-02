#if 0
tmpfile=$(mktemp)
cc "$0" -o "$tmpfile" && "$tmpfile" "$@"
exit
#endif

#include <errno.h>
#include <inttypes.h>
#include <stdio.h>

#include "../../c_utils/vec.h"

struct password {
    size_t first;
    size_t second;
    char req;
    char* password;
};

int read_password(FILE* file, struct password* password) {
    char buf[64];
    int result = fscanf(file, "%zu-%zu %c: %s ", &password->first,
                        &password->second, &password->req, buf);
    if (result == 4) {
        password->password = malloc(strlen(buf) + 1);
        strcpy(password->password, buf);
    }
    return result;
}

void password_done(struct password* password) {
    free(password->password);
}

typedef vec_t(struct password) password_vec_t;

void password_vec_done(password_vec_t* vec) {
    for (size_t i = 0; i < vec->length; i++) {
        password_done(&vec->data[i]);
    }

    vec_done(vec);
}

void read_input(char* filename, password_vec_t* vec) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error opening file %s: %d\n", filename, errno);
        exit(1);
    }

    struct password password;
    while (read_password(file, &password) != EOF) {
        vec_push(vec, password);
    }

    fclose(file);
}

void star1(password_vec_t* input) {
    size_t count = 0;
    for (size_t i = 0; i < input->length; i++) {
        struct password* password = &input->data[i];
        size_t char_count = 0;
        char* p = password->password;
        while (*p) {
            if (*p == password->req) {
                char_count++;
            }
            p++;
        }

        if (password->first <= char_count && char_count <= password->second) {
            count++;
        }
    }

    printf("%zu\n", count);
}

void star2(password_vec_t* input) {
    size_t count = 0;
    for (size_t i = 0; i < input->length; i++) {
        struct password* password = &input->data[i];
        size_t char_count = 0;
        int first = password->password[password->first - 1] == password->req;
        int second = password->password[password->second - 1] == password->req;

        if (first ^ second) {
            count++;
        }
    }

    printf("%zu\n", count);
}

int main(int argc, char** argv) {
    char* filename = "input.txt";
    if (argc > 1) {
        filename = argv[1];
    }

    password_vec_t input;
    vec_init(&input);

    read_input(filename, &input);

    star1(&input);
    star2(&input);

    password_vec_done(&input);
}
