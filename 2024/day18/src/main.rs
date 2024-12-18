use std::collections::HashSet;

use utils::{
    glam::{ivec2, IVec2},
    grid::von_neumann_neighbors,
    pathfinding::a_star,
    read_input_file,
};

fn parse_input(input: &str) -> Vec<IVec2> {
    input
        .lines()
        .map(|line| {
            let (x, y) = line.split_once(',').unwrap();
            ivec2(x.parse().unwrap(), y.parse().unwrap())
        })
        .collect()
}

fn star1(byte_locations: &[IVec2]) -> i64 {
    let bounds = ivec2(70, 70);
    let byte_locations: HashSet<_> = byte_locations[..1024].iter().copied().collect();

    a_star(
        ivec2(0, 0),
        |&pos| pos == bounds,
        |&pos| {
            von_neumann_neighbors(pos)
                .into_iter()
                .filter(|v| {
                    (0..=bounds.x).contains(&v.x)
                        && (0..=bounds.y).contains(&v.y)
                        && !byte_locations.contains(v)
                })
                .map(|v| (1, v))
                .collect()
        },
        |&pos| (pos.x.abs_diff(bounds.x) + (pos.y.abs_diff(bounds.y))) as _,
    )
    .unwrap()
    .0
}

fn star2(byte_locations: &[IVec2]) -> String {
    let bounds = ivec2(70, 70);
    let mut byte_locations_set: HashSet<_> = byte_locations[..1024].iter().copied().collect();

    let mut last_path = Vec::new();

    for &byte in byte_locations[1024..].iter() {
        byte_locations_set.insert(byte);
        if !last_path.is_empty() && !last_path.contains(&byte) {
            continue;
        }

        let a_star_result = a_star(
            ivec2(0, 0),
            |&pos| pos == bounds,
            |&pos| {
                von_neumann_neighbors(pos)
                    .into_iter()
                    .filter(|v| {
                        (0..=bounds.x).contains(&v.x)
                            && (0..=bounds.y).contains(&v.y)
                            && !byte_locations_set.contains(v)
                    })
                    .map(|v| (1, v))
                    .collect()
            },
            |&pos| (pos.x.abs_diff(bounds.x) + (pos.y.abs_diff(bounds.y))) as _,
        );

        match a_star_result {
            Some((_, path)) => last_path = path,
            None => return format!("{},{}", byte.x, byte.y),
        }
    }

    unreachable!("Path was not blocked at any point")
}

fn main() {
    let input = read_input_file!();
    let byte_locations = parse_input(&input);

    println!("{}", star1(&byte_locations));
    println!("{}", star2(&byte_locations));
}
