#if 0
tmpfile=$(mktemp)
cc "$0" -o "$tmpfile" && "$tmpfile" "$@"
exit
#endif

#include <errno.h>
#include <stdio.h>

#include "../c_utils/map.h"
#include "../c_utils/vec.h"

typedef enum {
    U,
    D,
    L,
    R,
} wire_seg_dir;

typedef struct {
    wire_seg_dir direction;
    int length;
} wire_seg_t;

typedef vec_t(wire_seg_t) vec_wire_seg_t;

typedef struct {
    int32_t x;
    int32_t y;
} xy_t;

uint64_t xy_t_hasher(const char* p) {
    return generic_hash(p, sizeof(xy_t));
}

int xy_t_eq(const char* p, const char* q) {
    const xy_t* a = (const xy_t*)p;
    const xy_t* b = (const xy_t*)q;
    return a->x == b->x && a->y == b->y;
}

typedef map_t(xy_t, int32_t) step_map_t;

void read_input(char* filename, vec_wire_seg_t* wire1, vec_wire_seg_t* wire2) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error opening file %s: %d\n", filename, errno);
        exit(1);
    }

    vec_wire_seg_t* curr_wire = wire1;
    char dir[2];
    int length;
    for (;;) {
        int read = fscanf(file, "%[UDLR]%d", dir, &length);
        if (read == EOF) {
            break;
        } else if (read == 0) {
            // switch to next line
            curr_wire = wire2;
            fscanf(file, " ");
            continue;
        } else {
            fscanf(file, ",");
        }

        wire_seg_t seg;
        switch (*dir) {
        case 'U':
            seg.direction = U;
            break;
        case 'D':
            seg.direction = D;
            break;
        case 'L':
            seg.direction = L;
            break;
        case 'R':
            seg.direction = R;
            break;
        default:
            printf("Unknown direction %c\n", *dir);
            exit(1);
        }
        seg.length = length;

        vec_push(curr_wire, seg);
    }

    fclose(file);
}

void get_wire_steps(vec_wire_seg_t* wire, step_map_t* step_map) {
    int32_t steps = 0;
    xy_t pos;
    pos.x = 0;
    pos.y = 0;
    for (size_t i = 0; i < wire->length; i++) {
        wire_seg_t* seg = &wire->data[i];
        for (size_t j = 0; j < seg->length; j++) {
            switch (seg->direction) {
            case U:
                pos.y--;
                break;
            case D:
                pos.y++;
                break;
            case L:
                pos.x--;
                break;
            case R:
                pos.x++;
                break;
            default:
                break;
            }
            steps++;
            map_insert(step_map, pos, steps);
        }
    }
}

void intersect_steps(step_map_t* m1, step_map_t* m2, step_map_t* out) {
    map_foreach(m1) {
        map_get(m2, *m1->key);
        if (m2->key != NULL) {
            xy_t key = *m1->key;
            int32_t value = *m1->value + *m2->value;
            map_insert(out, key, value);
        }
    }
}

void star1(step_map_t* intersection_map) {
    int32_t min = 0;
    map_foreach(intersection_map) {
        int32_t dist =
            abs(intersection_map->key->x) + abs(intersection_map->key->y);
        if (min == 0 || dist < min) {
            min = dist;
        }
    }
    printf("%d\n", min);
}

void star2(step_map_t* intersection_map) {
    int32_t min = 0;
    map_foreach(intersection_map) {
        int32_t steps = *intersection_map->value;
        if (min == 0 || steps < min) {
            min = steps;
        }
    }
    printf("%d\n", min);
}

int main(int argc, char** argv) {
    char* filename = "input.txt";
    if (argc > 1) {
        filename = argv[1];
    }

    vec_wire_seg_t wire1;
    vec_init(&wire1);
    vec_wire_seg_t wire2;
    vec_init(&wire2);

    read_input(filename, &wire1, &wire2);

    step_map_t step_map1;
    map_init(&step_map1, xy_t_hasher, xy_t_eq);

    get_wire_steps(&wire1, &step_map1);

    step_map_t step_map2;
    map_init(&step_map2, xy_t_hasher, xy_t_eq);

    get_wire_steps(&wire2, &step_map2);

    step_map_t intersection_map;
    map_init(&intersection_map, xy_t_hasher, xy_t_eq);

    intersect_steps(&step_map1, &step_map2, &intersection_map);

    star1(&intersection_map);
    star2(&intersection_map);

    map_done(&intersection_map);
    map_done(&step_map2);
    map_done(&step_map1);

    vec_done(&wire2);
    vec_done(&wire1);
}
