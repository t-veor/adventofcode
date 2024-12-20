use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use utils::{
    glam::IVec2,
    grid::{von_neumann_neighbors, CharGrid, DenseGrid},
    read_input_file,
};

fn get_distance_from_end(grid: &CharGrid, end: IVec2) -> DenseGrid<i32> {
    let mut distance_from_end = DenseGrid::new(grid.size(), |_| i32::MAX);

    let mut seen = HashSet::new();
    let mut frontier = vec![end];
    let mut next_frontier = Vec::new();
    let mut distance = 0;

    while !frontier.is_empty() {
        for pos in frontier.drain(..) {
            if !seen.insert(pos) {
                continue;
            }

            distance_from_end[pos] = distance;

            for neighbor in grid.von_neumann_neighbors(pos) {
                if grid[neighbor] == '.' {
                    next_frontier.push(neighbor);
                }
            }
        }

        distance += 1;
        std::mem::swap(&mut frontier, &mut next_frontier);
    }

    distance_from_end
}

fn reachable_spaces(start: IVec2, max_distance: i32) -> Vec<(IVec2, i32)> {
    thread_local! {
        static CACHE: RefCell<HashMap<i32, Vec<(IVec2, i32)>>> = RefCell::new(HashMap::new());
    }

    let reachable_from_origin = CACHE.with_borrow_mut(|cache| {
        cache
            .entry(max_distance)
            .or_insert_with(|| {
                let mut reached = Vec::new();

                let mut seen = HashSet::new();
                let mut frontier = vec![IVec2::ZERO];
                let mut next_frontier = Vec::new();

                for i in 0..=max_distance {
                    for pos in frontier.drain(..) {
                        if !seen.insert(pos) {
                            continue;
                        }

                        reached.push((pos, i));

                        for neighbor in von_neumann_neighbors(pos) {
                            next_frontier.push(neighbor);
                        }
                    }

                    std::mem::swap(&mut frontier, &mut next_frontier);
                }

                reached
            })
            .clone()
    });

    reachable_from_origin
        .into_iter()
        .map(|(pos, dist)| (pos + start, dist))
        .collect()
}

fn find_cheats_two(
    grid: &CharGrid,
    start: IVec2,
    end: IVec2,
    max_cheat_duration: i32,
) -> Vec<(i32, IVec2, IVec2)> {
    let distance_from_end = get_distance_from_end(grid, end);

    let mut cheats = Vec::new();

    let mut pos = start;
    let mut curr_time = distance_from_end[pos];
    'outer: while curr_time > 0 {
        // Look for places where we can skip across to a lower time
        for (skip_pos, skip_duration) in reachable_spaces(pos, max_cheat_duration) {
            match distance_from_end.get(skip_pos) {
                Some(&x) if x < curr_time - skip_duration => {
                    cheats.push((curr_time - skip_duration - x, pos, skip_pos))
                }
                _ => (),
            }
        }

        // Advance down the track
        for coord in distance_from_end.von_neumann_neighbors(pos) {
            if distance_from_end[coord] == curr_time - 1 {
                pos = coord;
                curr_time -= 1;
                continue 'outer;
            }
        }

        unreachable!()
    }

    cheats
}

fn parse_input(input: &str) -> (CharGrid, IVec2, IVec2) {
    let mut start = None;
    let mut end = None;

    let grid = CharGrid::read_from_str(input.trim(), |pos, c| match c {
        'S' => {
            start = Some(pos);
            '.'
        }
        'E' => {
            end = Some(pos);
            '.'
        }
        '#' | '.' => c,
        _ => unreachable!(),
    })
    .unwrap();

    (grid, start.unwrap(), end.unwrap())
}

fn star1(grid: &CharGrid, start: IVec2, end: IVec2) -> usize {
    find_cheats_two(grid, start, end, 2)
        .into_iter()
        .filter(|&(time_save, _, _)| time_save >= 100)
        .count()
}

fn star2(grid: &CharGrid, start: IVec2, end: IVec2) -> usize {
    find_cheats_two(grid, start, end, 20)
        .into_iter()
        .filter(|&(time_save, _, _)| time_save >= 100)
        .count()
}

fn main() {
    let input = read_input_file!();
    let (grid, start, end) = parse_input(&input);

    println!("{}", star1(&grid, start, end));
    println!("{}", star2(&grid, start, end));
}
