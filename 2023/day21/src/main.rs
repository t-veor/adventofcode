use core::panic;
use std::collections::HashSet;

use utils::{read_input_file, Direction};

fn parse_input(input: &str) -> Vec<Vec<u8>> {
    input.lines().map(|line| line.as_bytes().to_vec()).collect()
}

fn bfs_count_even(map: &[Vec<u8>], starting_pos: (isize, isize), max_steps: usize) -> usize {
    let width = map.first().map(|row| row.len()).unwrap_or(0) as isize;
    let height = map.len() as isize;

    let mut seen = HashSet::new();
    let mut seen_even = 0;

    let mut frontier = vec![starting_pos];
    let mut next_frontier = Vec::new();

    seen.insert(starting_pos);

    for steps in 0..=max_steps {
        if steps % 2 == max_steps % 2 {
            seen_even += frontier.len();
        }

        if steps == max_steps {
            break;
        }

        for pos in frontier.drain(..) {
            for dir in Direction::ALL {
                let (x, y) = dir.step(pos);
                if !seen.insert((x, y)) {
                    continue;
                }

                if map[(y.rem_euclid(height)) as usize][x.rem_euclid(width) as usize] != b'#' {
                    next_frontier.push((x, y));
                }
            }
        }

        std::mem::swap(&mut frontier, &mut next_frontier);
    }

    seen_even
}

fn starting_pos(map: &[Vec<u8>]) -> (isize, isize) {
    for (y, row) in map.iter().enumerate() {
        for (x, c) in row.iter().enumerate() {
            if *c == b'S' {
                return (x as isize, y as isize);
            }
        }
    }

    panic!("No starting pos")
}

fn star1(input: &[Vec<u8>]) -> usize {
    bfs_count_even(input, starting_pos(input), 64)
}

fn extrapolate_quadratic(initial_values: [i64; 3], n: i64) -> i64 {
    // Assume tn = a n^2 + b n + c
    let [t0, t1, t2] = initial_values;
    // Then, un = t{n+1} - tn = 2a n + a + b
    let (u0, u1) = (t1 - t0, t2 - t1);
    // Then, vn = u{n+1} - un = 2a
    let v0 = u1 - u0;

    let two_a = v0;
    let two_b = 2 * u0 - two_a;
    let c = t0;

    // Compute a n^2 + b n + c
    (two_a * n * n + two_b * n) / 2 + c
}

fn extrapolated_grid_count(input: &[Vec<u8>], total_count: usize) -> i64 {
    let even_grid_size = input.len() * 2;
    let starting_pos = starting_pos(input);

    let x0 = bfs_count_even(input, starting_pos, total_count % even_grid_size) as i64;
    let x1 = bfs_count_even(
        input,
        starting_pos,
        total_count % even_grid_size + even_grid_size,
    ) as i64;
    let x2 = bfs_count_even(
        input,
        starting_pos,
        total_count % even_grid_size + 2 * even_grid_size,
    ) as i64;

    extrapolate_quadratic([x0, x1, x2], (total_count / even_grid_size) as i64)
}

fn star2(input: &[Vec<u8>]) -> i64 {
    // Unhappy with this. Makes a couple of assumptions about the input (which
    // aren't even true for the example input):
    // * The row/column the starting element is on is empty, meaning the
    //   shortest distance between two map corners in the infinite grid is simply
    //   the manhattan distance
    // * The grid is "sufficiently empty" such that basically walking 65 steps
    //   covers all the reachable squares, causing the grid to grow in a
    //   predictable way
    // Still, this seems to be the intended way and also the way that works for
    // different puzzle inputs...
    extrapolated_grid_count(input, 26501365)
}

fn main() {
    let input = read_input_file!();
    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
