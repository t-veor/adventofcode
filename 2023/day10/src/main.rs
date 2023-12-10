use std::collections::{HashMap, HashSet, VecDeque};

use bitflags::bitflags;
use utils::read_input_file;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Dir: u8 {
        const NORTH = 0b0001;
        const SOUTH = 0b0010;
        const EAST = 0b0100;
        const WEST = 0b1000;
    }
}

impl Dir {
    const DIRS: [Self; 4] = [Dir::NORTH, Dir::SOUTH, Dir::EAST, Dir::WEST];

    fn flip(self) -> Self {
        let mut result = Dir::empty();
        if self.contains(Dir::NORTH) {
            result |= Dir::SOUTH;
        }
        if self.contains(Dir::SOUTH) {
            result |= Dir::NORTH;
        }
        if self.contains(Dir::EAST) {
            result |= Dir::WEST;
        }
        if self.contains(Dir::WEST) {
            result |= Dir::EAST;
        }
        result
    }

    fn delta(self) -> Option<(isize, isize)> {
        match self {
            Self::NORTH => Some((0, -1)),
            Self::SOUTH => Some((0, 1)),
            Self::EAST => Some((1, 0)),
            Self::WEST => Some((-1, 0)),
            _ => None,
        }
    }

    fn pick(self) -> Self {
        Self::from_bits_truncate(self.bits() & (1 << self.bits().trailing_zeros()))
    }
}

fn connections(c: u8) -> Dir {
    match c {
        b'|' => Dir::NORTH | Dir::SOUTH,
        b'-' => Dir::EAST | Dir::WEST,
        b'L' => Dir::NORTH | Dir::EAST,
        b'J' => Dir::NORTH | Dir::WEST,
        b'7' => Dir::SOUTH | Dir::WEST,
        b'F' => Dir::SOUTH | Dir::EAST,
        // S is handled separately
        _ => Dir::empty(),
    }
}

struct PipeGrid {
    grid: Vec<Vec<u8>>,
    size: (isize, isize),
    start_pos: (isize, isize),
    start_connections: Dir,
}

impl PipeGrid {
    fn parse(input: &str) -> Self {
        let grid: Vec<_> = input.lines().map(|line| line.as_bytes().to_vec()).collect();

        let width = grid.first().map(|l| l.len()).unwrap_or(0);
        let height = grid.len();

        let start_pos = grid
            .iter()
            .enumerate()
            .find_map(|(row, line)| {
                line.iter()
                    .position(|&c| c == b'S')
                    .map(|col| (col as isize, row as isize))
            })
            .unwrap();

        let start_connections = Dir::DIRS
            .into_iter()
            .filter(|dir| {
                let delta = dir.delta().unwrap();
                let next_pos = (start_pos.0 + delta.0, start_pos.1 + delta.1);

                if next_pos.0 < 0 || next_pos.1 < 0 {
                    return false;
                }

                grid.get(next_pos.1 as usize)
                    .and_then(|row| row.get(next_pos.0 as usize))
                    .copied()
                    .map(|c| connections(c).contains(dir.flip()))
                    .unwrap_or(false)
            })
            .fold(Dir::empty(), |a, b| a | b);

        Self {
            grid,
            size: (width as isize, height as isize),
            start_pos,
            start_connections,
        }
    }

    fn get(&self, (col, row): (isize, isize)) -> Option<u8> {
        if col < 0 || row < 0 {
            return None;
        }

        self.grid
            .get(row as usize)
            .and_then(|r| r.get(col as usize))
            .copied()
    }

    fn connections_at(&self, pos: (isize, isize)) -> Dir {
        self.get(pos)
            .map(|c| {
                if c == b'S' {
                    self.start_connections
                } else {
                    connections(c)
                }
            })
            .unwrap_or(Dir::empty())
    }

    fn in_bounds(&self, (x, y): (isize, isize)) -> bool {
        let (width, height) = self.size;
        (0..width).contains(&x) && (0..height).contains(&y)
    }

    fn adjacent(&self, doubled: DoubledCoords) -> Vec<DoubledCoords> {
        let mut result = Vec::with_capacity(4);

        let connections = self.connections_at(doubled.to_coords());
        let (qx, qy) = doubled.quadrant();

        if qx == 1
            || qy == 0 && !connections.contains(Dir::NORTH)
            || qy == 1 && !connections.contains(Dir::SOUTH)
        {
            // east
            result.push(DoubledCoords {
                x: doubled.x + 1,
                ..doubled
            });
        }

        if qx == 0
            || qy == 0 && !connections.contains(Dir::NORTH)
            || qy == 1 && !connections.contains(Dir::SOUTH)
        {
            // west
            result.push(DoubledCoords {
                x: doubled.x - 1,
                ..doubled
            });
        }

        if qy == 1
            || qx == 0 && !connections.contains(Dir::WEST)
            || qx == 1 && !connections.contains(Dir::EAST)
        {
            // south
            result.push(DoubledCoords {
                y: doubled.y + 1,
                ..doubled
            })
        }

        if qy == 0
            || qx == 0 && !connections.contains(Dir::WEST)
            || qx == 1 && !connections.contains(Dir::EAST)
        {
            // north
            result.push(DoubledCoords {
                y: doubled.y - 1,
                ..doubled
            })
        }

        result
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct DoubledCoords {
    x: isize,
    y: isize,
}

impl DoubledCoords {
    fn from_coords(pos: (isize, isize), quadrant: (isize, isize)) -> Self {
        Self {
            x: 2 * pos.0 + quadrant.0,
            y: 2 * pos.1 + quadrant.1,
        }
    }

    fn to_coords(self) -> (isize, isize) {
        (self.x.div_euclid(2), self.y.div_euclid(2))
    }

    fn quadrant(self) -> (isize, isize) {
        (self.x.rem_euclid(2), self.y.rem_euclid(2))
    }
}

fn star1(grid: &PipeGrid) -> i32 {
    let mut next_dir = grid.start_connections.pick();
    let mut pos = grid.start_pos;
    let mut steps = 0;
    loop {
        let delta = next_dir.delta().unwrap();
        pos = (pos.0 + delta.0, pos.1 + delta.1);
        steps += 1;

        let connections = grid.connections_at(pos);
        next_dir = connections.difference(next_dir.flip());

        if pos == grid.start_pos {
            break;
        }
    }

    steps / 2
}

fn find_enclosed_area(grid: &PipeGrid, start_pos: DoubledCoords) -> Option<usize> {
    let mut seen = HashSet::new();
    let mut frontier = VecDeque::new();

    seen.insert(start_pos);
    frontier.push_back(start_pos);

    while let Some(pos) = frontier.pop_front() {
        if !grid.in_bounds(pos.to_coords()) {
            return None;
        }

        for adjacent in grid.adjacent(pos) {
            if seen.insert(adjacent) {
                frontier.push_back(adjacent);
            }
        }
    }

    // Count grid cells where all 4 quadrants are accessible.
    let mut accessible_quadrant_count = HashMap::new();

    for doubled_coords in seen {
        let pos = doubled_coords.to_coords();
        *accessible_quadrant_count.entry(pos).or_insert(0) += 1;
    }

    Some(
        accessible_quadrant_count
            .values()
            .filter(|&&v| v >= 4)
            .count(),
    )
}

fn star2(grid: &PipeGrid) -> usize {
    for quadrant in [(0, 0), (0, 1), (1, 0), (1, 1)] {
        let start_pos = DoubledCoords::from_coords(grid.start_pos, quadrant);
        if let Some(enclosed_area) = find_enclosed_area(grid, start_pos) {
            return enclosed_area;
        }
    }

    panic!()
}

fn main() {
    let input = read_input_file!();
    let grid = PipeGrid::parse(&input);

    println!("{}", star1(&grid));
    println!("{}", star2(&grid));
}
