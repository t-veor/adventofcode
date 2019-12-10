#if 0
tmpfile=$(mktemp)
cc "$0" -o "$tmpfile" && "$tmpfile" "$@"
exit
#endif

#include <errno.h>
#include <stdio.h>
#include <inttypes.h>

#include "../c_utils/vec.h"
#include "../c_utils/map.h"

typedef struct {
    char label[4];
} orbit_label_t;

uint64_t orbit_label_t_hasher(const char* p) {
    const orbit_label_t* a = (const orbit_label_t*)p;
    return generic_hash(a->label, strlen(a->label));
}

int orbit_label_t_eq(const char* p, const char* q) {
    const orbit_label_t* a = (const orbit_label_t*)p;
    const orbit_label_t* b = (const orbit_label_t*)q;
    return strcmp(a->label, b->label) == 0;
}

typedef vec_t(orbit_label_t) vec_orbit_label_t;
typedef map_t(orbit_label_t, orbit_label_t) orbit_tree_t;

void path_from_root(orbit_tree_t* tree, const orbit_label_t* node,
                    vec_orbit_label_t* out) {
    for (;;) {
        map_get(tree, *node);
        if (tree->value == NULL) {
            break;
        }
        node = tree->value;
        vec_push(out, *node);
    }
    vec_reverse(out);
}

int32_t shortest_path_between(orbit_tree_t* tree, const orbit_label_t* start,
                              const orbit_label_t* end) {
    vec_orbit_label_t path1;
    vec_init(&path1);
    vec_orbit_label_t path2;
    vec_init(&path2);

    path_from_root(tree, start, &path1);
    path_from_root(tree, end, &path2);

    int32_t common_subpath_length = 0;
    for (size_t i = 0; ; i++) {
        if (i >= path1.length || i >= path2.length) {
            break;
        }

        if (strcmp(path1.data[i].label, path2.data[i].label) != 0) {
            break;
        }
        common_subpath_length++;
    }

    int32_t result = (int32_t)path1.length + (int32_t)path2.length
        - 2 * common_subpath_length;

    vec_done(&path2);
    vec_done(&path1);

    return result;
}

void read_input(char* filename, orbit_tree_t* out) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error opening file %s: %d\n", filename, errno);
        exit(1);
    }

    orbit_label_t parent;
    orbit_label_t child;
    while (fscanf(file, "%3[^)\n])%3[^)\n] ", parent.label, child.label) == 2) {
        map_insert(out, child, parent);
    }

    fclose(file);
}

void star1(orbit_tree_t* tree) {
    vec_orbit_label_t path;
    vec_init(&path);

    size_t count = 0;
    map_foreach(tree) {
        vec_clear(&path);
        path_from_root(tree, tree->key, &path);
        count += path.length;
    }

    printf("%zu\n", count);

    vec_done(&path);
}

void star2(orbit_tree_t* tree) {
    orbit_label_t start = { "YOU" };
    orbit_label_t end = { "SAN" };

    printf("%" PRId32 "\n", shortest_path_between(tree, &start, &end));
}

int main(int argc, char** argv) {
    char* filename = "input.txt";
    if (argc > 1) {
        filename = argv[1];
    }

    orbit_tree_t tree;
    map_init(&tree, orbit_label_t_hasher, orbit_label_t_eq);

    read_input(filename, &tree);

    star1(&tree);
    star2(&tree);

    map_done(&tree);
}
