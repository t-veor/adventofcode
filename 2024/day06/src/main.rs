use std::collections::HashSet;

use utils::{
    glam::IVec2,
    grid::{CharGrid, OrthoDir},
    read_input_file,
};

fn parse_input(input: &str) -> (CharGrid, IVec2) {
    let mut grid = CharGrid::from_str_chars(input).unwrap();
    let starting_position = grid.find(&'^').unwrap();
    *grid.get_mut(starting_position).unwrap() = '.';

    (grid, starting_position)
}

#[derive(Debug)]
enum PatrolResult {
    Exited(HashSet<IVec2>),
    Looped,
}

fn guard_patrol(grid: &CharGrid, starting_pos: IVec2) -> PatrolResult {
    let mut visited = HashSet::new();
    let mut visited_states = HashSet::new();

    let mut guard_pos = starting_pos;
    let mut guard_facing = OrthoDir::UP;

    loop {
        visited.insert(guard_pos);
        if !visited_states.insert((guard_pos, guard_facing)) {
            // Seen this state before
            return PatrolResult::Looped;
        }

        let pos_in_front = guard_facing.step(guard_pos);
        match grid.get(pos_in_front) {
            // Turn 90 deg
            Some('#') => guard_facing = guard_facing.rotate_cw(),
            // Step right
            Some(_) => guard_pos = pos_in_front,
            // Exited map!
            None => break,
        }
    }

    PatrolResult::Exited(visited)
}

fn star1(grid: &CharGrid, starting_pos: IVec2) -> usize {
    match guard_patrol(grid, starting_pos) {
        PatrolResult::Exited(visited) => visited.len(),
        PatrolResult::Looped => panic!("Loop in starting input?"),
    }
}

fn star2(grid: &CharGrid, starting_pos: IVec2) -> usize {
    // Do an initial scan to find squares for which putting an obstruction would
    // do anything
    let visited = match guard_patrol(grid, starting_pos) {
        PatrolResult::Exited(visited) => visited,
        PatrolResult::Looped => panic!("Loop in starting input?"),
    };

    let mut loop_positions = 0;

    for coord in visited {
        if coord == starting_pos {
            continue;
        }

        let mut grid = grid.clone();
        // Try changing the current square to a #
        let curr_square = grid.get_mut(coord).unwrap();
        if *curr_square == '#' {
            // Already obstruction, skip
            continue;
        }
        *curr_square = '#';

        match guard_patrol(&grid, starting_pos) {
            PatrolResult::Exited(_) => (),
            PatrolResult::Looped => loop_positions += 1,
        }
    }

    loop_positions
}

fn main() {
    let input = read_input_file!();
    let (grid, starting_pos) = parse_input(&input);

    println!("{}", star1(&grid, starting_pos));
    println!("{}", star2(&grid, starting_pos));
}
