#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::{HashSet, VecDeque};

fn parse_input(input: String) -> Vec<(i32, i32, i32)> {
    input
        .lines()
        .map(|line| {
            let (x, yz) = line.split_once(',').unwrap();
            let (y, z) = yz.split_once(',').unwrap();
            (x.parse().unwrap(), y.parse().unwrap(), z.parse().unwrap())
        })
        .collect()
}

fn adjacent((x, y, z): (i32, i32, i32)) -> [(i32, i32, i32); 6] {
    [
        (x + 1, y, z),
        (x - 1, y, z),
        (x, y + 1, z),
        (x, y - 1, z),
        (x, y, z + 1),
        (x, y, z - 1),
    ]
}

fn star1(input: &[(i32, i32, i32)]) -> i32 {
    let cubes: HashSet<_> = input.iter().copied().collect();
    let mut surface_area = 0;

    // Each cube adds 6 units of surface area
    surface_area += 6 * input.len() as i32;

    for cube in cubes.iter().copied() {
        // If there is an adjacent cube, remove 1 from the number of faces
        for adjacent_cube in adjacent(cube) {
            if cubes.contains(&adjacent_cube) {
                surface_area -= 1;
            }
        }
    }

    surface_area
}

fn star2(input: &[(i32, i32, i32)]) -> i32 {
    let cubes: HashSet<_> = input.iter().copied().collect();

    let min_x = input.iter().map(|(x, _, _)| *x).min().unwrap();
    let max_x = input.iter().map(|(x, _, _)| *x).max().unwrap();
    let min_y = input.iter().map(|(_, y, _)| *y).min().unwrap();
    let max_y = input.iter().map(|(_, y, _)| *y).max().unwrap();
    let min_z = input.iter().map(|(_, _, z)| *z).min().unwrap();
    let max_z = input.iter().map(|(_, _, z)| *z).max().unwrap();

    let x_range = min_x - 1..max_x + 2;
    let y_range = min_y - 1..max_y + 2;
    let z_range = min_z - 1..max_z + 2;

    // BFS over the enclosing cuboid to discover all cubes on reachable on the
    // outside of the lava droplet
    let mut reachable = HashSet::new();
    let mut queue = VecDeque::new();

    let root = (x_range.start, y_range.start, z_range.start);
    reachable.insert(root);
    queue.push_back(root);

    while let Some(cube) = queue.pop_front() {
        for (x, y, z) in adjacent(cube) {
            if x_range.contains(&x)
                && y_range.contains(&y)
                && z_range.contains(&z)
                && !cubes.contains(&(x, y, z))
            {
                if reachable.insert((x, y, z)) {
                    queue.push_back((x, y, z))
                }
            }
        }
    }

    // Now, work out the surface area (including internal) of this big enclosing
    // cuboid
    let mut surface_area = 6 * reachable.len() as i32;
    for cube in reachable.iter().copied() {
        for adjacent_cube in adjacent(cube) {
            if reachable.contains(&adjacent_cube) {
                surface_area -= 1;
            }
        }
    }

    // The external surface area of the lava droplet is equal to the internal
    // surface area of this big enclosing cuboid, which is just the surface area
    // minus the external surface area, which is easy to calculate
    let width = x_range.len() as i32;
    let height = y_range.len() as i32;
    let depth = z_range.len() as i32;
    let external_surface_area = 2 * (width * height + width * depth + height * depth);
    surface_area - external_surface_area
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
