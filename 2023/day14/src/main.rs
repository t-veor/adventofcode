use std::collections::HashMap;

use utils::read_input_file;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Rocks {
    size: (usize, usize),
    rocks: Vec<Vec<u8>>,
}

#[derive(Debug, Clone, Copy)]
enum RollDirection {
    North,
    East,
    South,
    West,
}

impl Rocks {
    fn parse_input(input: &str) -> Self {
        let rocks: Vec<_> = input.lines().map(|line| line.as_bytes().to_vec()).collect();
        let width = rocks.first().unwrap().len();
        let height = rocks.len();

        Self {
            size: (width, height),
            rocks,
        }
    }

    fn effective_size(&self, roll_dir: RollDirection) -> (isize, isize) {
        let (width, height) = self.size;
        match roll_dir {
            RollDirection::North | RollDirection::South => (width as _, height as _),
            RollDirection::East | RollDirection::West => (height as _, width as _),
        }
    }

    fn transform_coords(&self, (x, y): (isize, isize), roll_dir: RollDirection) -> (usize, usize) {
        let (width, height) = self.size;
        let (x, y) = (x.try_into().unwrap(), y.try_into().unwrap());

        match roll_dir {
            RollDirection::North => (x, y),
            RollDirection::East => (width - y - 1, x),
            RollDirection::South => (width - x - 1, height - y - 1),
            RollDirection::West => (y, height - x - 1),
        }
    }

    fn get(&self, coords: (isize, isize), roll_dir: RollDirection) -> u8 {
        let (x, y) = self.transform_coords(coords, roll_dir);
        self.rocks[y][x]
    }

    fn swap(&mut self, from: (isize, isize), to: (isize, isize), roll_dir: RollDirection) {
        let (from_x, from_y) = self.transform_coords(from, roll_dir);
        let (to_x, to_y) = self.transform_coords(to, roll_dir);

        let tmp = self.rocks[from_y][from_x];
        self.rocks[from_y][from_x] = self.rocks[to_y][to_x];
        self.rocks[to_y][to_x] = tmp;
    }

    fn roll(&mut self, roll_dir: RollDirection) {
        let (width, height) = self.effective_size(roll_dir);
        let mut highest_empty_space_in_col = vec![0; width as usize];

        for y in 0..height {
            for x in 0..width {
                match self.get((x, y), roll_dir) {
                    b'O' => {
                        let empty_row = highest_empty_space_in_col[x as usize];
                        self.swap((x, empty_row), (x, y), roll_dir);
                        highest_empty_space_in_col[x as usize] = empty_row + 1;
                    }
                    b'#' => highest_empty_space_in_col[x as usize] = y + 1,
                    _ => (),
                }
            }
        }
    }

    fn spin_cycle(&mut self) {
        self.roll(RollDirection::North);
        self.roll(RollDirection::West);
        self.roll(RollDirection::South);
        self.roll(RollDirection::East);
    }

    fn calc_load_north(&self) -> i64 {
        self.rocks
            .iter()
            .enumerate()
            .map(|(y, row)| {
                let distance_to_south_edge = (self.rocks.len() - y) as i64;
                row.iter().filter(|&&x| x == b'O').count() as i64 * distance_to_south_edge
            })
            .sum()
    }

    #[allow(unused)]
    fn debug_print(&self) {
        for row in self.rocks.iter() {
            println!("{}", String::from_utf8(row.to_vec()).unwrap());
        }
    }
}

fn star1(mut rocks: Rocks) -> i64 {
    rocks.roll(RollDirection::North);
    rocks.calc_load_north()
}

fn star2(mut rocks: Rocks) -> i64 {
    const TARGET_CYCLES: i64 = 1_000_000_000;

    let mut seen_rocks = HashMap::new();
    seen_rocks.insert(rocks.clone(), 0i64);

    let mut skipped = false;
    let mut curr_cycles = 1;

    while curr_cycles <= TARGET_CYCLES {
        let mut next_rocks = rocks.clone();
        next_rocks.spin_cycle();

        if !skipped {
            if let Some(prev_cycles) = seen_rocks.insert(next_rocks.clone(), curr_cycles) {
                let cycle_length = curr_cycles - prev_cycles;

                let remaining = TARGET_CYCLES - curr_cycles;
                let skip_amount = (remaining / cycle_length) * cycle_length;

                curr_cycles += skip_amount;
                skipped = true;
            }
        }

        rocks = next_rocks;
        curr_cycles += 1;
    }

    rocks.calc_load_north()
}

fn main() {
    let input = read_input_file!();
    let rocks = Rocks::parse_input(&input);

    println!("{}", star1(rocks.clone()));
    println!("{}", star2(rocks));
}
