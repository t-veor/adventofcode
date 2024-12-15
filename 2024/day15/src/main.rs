use std::collections::HashSet;

use utils::{
    glam::{ivec2, IVec2},
    grid::{DenseGrid, OrthoDir},
    read_input_file,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Square {
    Empty,
    Wall,
    Box,
}

fn try_move(grid: &mut DenseGrid<Square>, robot_pos: &mut IVec2, dir: OrthoDir) -> bool {
    let robot_next_square = dir.step(*robot_pos);
    let mut next_square = robot_next_square;

    loop {
        match grid.get(next_square).unwrap_or(&Square::Wall) {
            Square::Empty => {
                // Moving into empty space, OK
                // Since the only thing that can move is boxes, we're fine just
                // swapping the box at the robot's next intended position to
                // this empty square. (This even works if there were no boxes
                // to push because then we're just swapping an empty square with
                // itself.)
                (grid[robot_next_square], grid[next_square]) =
                    (grid[next_square], grid[robot_next_square]);
                *robot_pos = robot_next_square;
                return true;
            }
            Square::Box => {
                // Moving into a box -- continue searching forward to see if a
                // push is possible
                next_square = dir.step(next_square)
            }
            Square::Wall => {
                // No move or push possible, exit without changing anything
                return false;
            }
        }
    }
}

fn run_robot_instructions(
    grid: &mut DenseGrid<Square>,
    mut robot_pos: IVec2,
    instructions: &[OrthoDir],
) {
    for &instruction in instructions {
        try_move(grid, &mut robot_pos, instruction);
    }
}

#[allow(unused)]
fn debug_print(grid: &DenseGrid<Square>, robot_pos: IVec2) {
    for y in 0..grid.height() {
        for x in 0..grid.width() {
            let coords = ivec2(x, y);
            let char = match grid[coords] {
                Square::Box => 'O',
                Square::Empty if coords == robot_pos => '@',
                Square::Empty => '.',
                Square::Wall => '#',
            };

            print!("{char}");
        }
        println!();
    }
}

struct DoubleWideMap {
    walls: HashSet<IVec2>,
    boxes: HashSet<IVec2>,
    bounds: IVec2,
}

impl DoubleWideMap {
    fn new(grid: &DenseGrid<Square>) -> Self {
        let bounds = ivec2(grid.width() * 2, grid.height());

        let mut walls = HashSet::new();
        let mut boxes = HashSet::new();

        for coord in grid.coord_iter() {
            let mapped_coord = ivec2(coord.x * 2, coord.y);
            match grid[coord] {
                Square::Empty => (),
                Square::Wall => {
                    walls.insert(mapped_coord);
                    walls.insert(mapped_coord + IVec2::X);
                }
                Square::Box => {
                    boxes.insert(mapped_coord);
                }
            }
        }

        Self {
            walls,
            boxes,
            bounds,
        }
    }

    fn is_wall(&self, pos: IVec2) -> bool {
        if !(0..self.bounds.x).contains(&pos.x) || !(0..self.bounds.y).contains(&pos.y) {
            return true;
        }

        self.walls.contains(&pos)
    }

    fn get_box(&self, pos: IVec2) -> Option<(IVec2, IVec2)> {
        if self.boxes.contains(&pos) {
            Some((pos, pos + IVec2::X))
        } else if self.boxes.contains(&(pos - IVec2::X)) {
            Some((pos - IVec2::X, pos))
        } else {
            None
        }
    }

    fn try_move(&mut self, robot_pos: &mut IVec2, dir: OrthoDir) -> bool {
        let mut boxes_in_push_chain = Vec::new();
        // List of spaces that need to be empty for the move to succeed
        let mut space_check_list = Vec::new();

        let robot_next_pos = dir.step(*robot_pos);
        space_check_list.push(robot_next_pos);

        while let Some(space_to_check) = space_check_list.pop() {
            if self.is_wall(space_to_check) {
                // Failed move, return without changing anything
                return false;
            } else if let Some((box_left, box_right)) = self.get_box(space_to_check) {
                // If the box was already in the push chain, we've resolved this box already
                if !boxes_in_push_chain.contains(&box_left) {
                    boxes_in_push_chain.push(box_left);

                    // Move may still be possible if the box can be pushed...
                    if dir.step(box_left) != box_right {
                        space_check_list.push(dir.step(box_left));
                    }
                    if dir.step(box_right) != box_left {
                        space_check_list.push(dir.step(box_right));
                    }
                }
            } else {
                // This space is empty! We don't need to do any further checks
                // from this space.
            }
        }

        // Getting here means all the spaces to check have been resolved and the
        // move is possible!
        // Move all the boxes in the push chain in the target direction
        for box_pos in boxes_in_push_chain.iter() {
            self.boxes.remove(&box_pos);
        }

        for box_pos in boxes_in_push_chain {
            self.boxes.insert(dir.step(box_pos));
        }

        // And step the robot in the target direction
        *robot_pos = robot_next_pos;

        true
    }

    fn run_robot_instructions(&mut self, mut robot_pos: IVec2, instructions: &[OrthoDir]) {
        for &instruction in instructions {
            self.try_move(&mut robot_pos, instruction);
        }
    }

    #[allow(unused)]
    fn debug_print(&self, robot_pos: IVec2) {
        for y in 0..self.bounds.y {
            for x in 0..self.bounds.x {
                let pos = ivec2(x, y);

                let char = if self.is_wall(pos) {
                    '#'
                } else if let Some((box_left, _box_right)) = self.get_box(pos) {
                    if box_left == pos {
                        '['
                    } else {
                        ']'
                    }
                } else if pos == robot_pos {
                    '@'
                } else {
                    '.'
                };

                print!("{char}");
            }
            println!();
        }
    }
}

fn parse_input(input: &str) -> (DenseGrid<Square>, IVec2, Vec<OrthoDir>) {
    let (map, instructions) = input.split_once("\n\n").unwrap();

    let mut robot_position = None;

    let map = DenseGrid::read_from_str(map, |pos, c| match c {
        '.' => Square::Empty,
        '#' => Square::Wall,
        'O' => Square::Box,

        '@' => {
            robot_position = Some(pos);
            Square::Empty
        }

        _ => unreachable!(),
    })
    .unwrap();
    let instructions = instructions
        .chars()
        .filter_map(|c| match c {
            '^' => Some(OrthoDir::UP),
            '>' => Some(OrthoDir::RIGHT),
            'v' => Some(OrthoDir::DOWN),
            '<' => Some(OrthoDir::LEFT),
            _ => None,
        })
        .collect();

    (map, robot_position.unwrap(), instructions)
}

fn star1(grid: &DenseGrid<Square>, robot_pos: IVec2, instructions: &[OrthoDir]) -> i32 {
    let mut grid = grid.clone();
    run_robot_instructions(&mut grid, robot_pos, instructions);

    let mut total = 0;
    for coord in grid.coord_iter() {
        if grid[coord] == Square::Box {
            total += coord.y * 100 + coord.x;
        }
    }
    total
}

fn star2(grid: &DenseGrid<Square>, robot_pos: IVec2, instructions: &[OrthoDir]) -> i32 {
    let mut grid = DoubleWideMap::new(grid);
    let robot_pos = ivec2(robot_pos.x * 2, robot_pos.y);
    grid.run_robot_instructions(robot_pos, instructions);

    let mut total = 0;
    for coord in grid.boxes.iter().copied() {
        total += coord.y * 100 + coord.x;
    }
    total
}

fn main() {
    let input = read_input_file!();
    let (grid, robot_pos, instructions) = parse_input(&input);

    println!("{}", star1(&grid, robot_pos, &instructions));
    println!("{}", star2(&grid, robot_pos, &instructions));
}
