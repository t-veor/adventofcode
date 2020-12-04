#if 0
tmpfile=$(mktemp)
cc "$0" -o "$tmpfile" && "$tmpfile" "$@"
exit
#endif

#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>

#include "../../c_utils/vec.h"

typedef vec_t(bool) bool_vec_t;
typedef vec_t(bool_vec_t) tree_grid_t;

void tree_grid_done(tree_grid_t* grid) {
    for (size_t i = 0; i < grid->length; i++) {
        vec_done(&grid->data[i]);
    }
    vec_done(grid);
}

void read_input(char* filename, tree_grid_t* grid) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error opening file %s: %d\n", filename, errno);
        exit(1);
    }

    char buf[1024];
    bool_vec_t vec;
    while (fgets(buf, sizeof(buf), file) != NULL) {
        vec_init(&vec);

        char* p = buf;
        while (*p) {
            if (*p == '\n') {
                break;
            }

            vec_push(&vec, *p == '#');
            p++;
        }

        vec_push(grid, vec);
    }

    fclose(file);
}

int count_trees(tree_grid_t* grid, int dx, int dy) {
    int x = 0;
    int y = 0;
    int count = 0;
    while (y < grid->length) {
        bool_vec_t* row = &grid->data[y];
        if (row->data[x % row->length]) {
            count++;
        }
        x += dx;
        y += dy;
    }
    return count;
}

void star1(tree_grid_t* grid) {
    printf("%d\n", count_trees(grid, 3, 1));
}

void star2(tree_grid_t* grid) {
    int product = 1;
    product *= count_trees(grid, 1, 1);
    product *= count_trees(grid, 3, 1);
    product *= count_trees(grid, 5, 1);
    product *= count_trees(grid, 7, 1);
    product *= count_trees(grid, 1, 2);
    printf("%d\n", product);
}

int main(int argc, char** argv) {
    char* filename = "input.txt";
    if (argc > 1) {
        filename = argv[1];
    }

    tree_grid_t input;
    vec_init(&input);

    read_input(filename, &input);

    star1(&input);
    star2(&input);

    tree_grid_done(&input);
}
