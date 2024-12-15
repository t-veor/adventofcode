use std::collections::HashSet;

use utils::{
    glam::IVec2,
    grid::{von_neumann_neighbors, DenseGrid},
    read_input_file,
};

fn dfs_find_trails(grid: &DenseGrid<u8>, start: IVec2) -> (u32, u32) {
    fn go(
        grid: &DenseGrid<u8>,
        curr: IVec2,
        trail_ends_found: &mut HashSet<IVec2>,
        paths_found: &mut u32,
    ) {
        // Don't need a visited_nodes structure because all previously visited
        // nodes can't be visited again as they are of lower height
        let curr_height = *grid.get(curr).unwrap();

        if curr_height == 9 {
            trail_ends_found.insert(curr);
            *paths_found += 1;
            return;
        }

        for neighbor in von_neumann_neighbors(curr) {
            match grid.get(neighbor) {
                Some(x) if curr_height + 1 == *x => {
                    go(grid, neighbor, trail_ends_found, paths_found)
                }
                _ => (),
            }
        }
    }

    let mut trail_ends_found = HashSet::new();
    let mut paths_found = 0;
    go(grid, start, &mut trail_ends_found, &mut paths_found);

    (trail_ends_found.len() as u32, paths_found)
}

fn parse_input(input: &str) -> DenseGrid<u8> {
    DenseGrid::read_from_str(input, |_, c| {
        if ('0'..='9').contains(&c) {
            c as u8 - '0' as u8
        } else {
            128
        }
    })
    .unwrap()
}

fn star1(grid: &DenseGrid<u8>) -> u32 {
    let mut total = 0;

    for coord in grid.coord_iter() {
        if grid[coord] == 0 {
            let reachable_9s = dfs_find_trails(grid, coord).0;
            total += reachable_9s;
        }
    }

    total
}

fn star2(grid: &DenseGrid<u8>) -> u32 {
    let mut total = 0;

    for coord in grid.coord_iter() {
        if grid[coord] == 0 {
            let distinct_trails = dfs_find_trails(grid, coord).1;
            total += distinct_trails;
        }
    }

    total
}

fn main() {
    let input = read_input_file!();
    let grid = parse_input(&input);

    println!("{}", star1(&grid));
    println!("{}", star2(&grid));
}
