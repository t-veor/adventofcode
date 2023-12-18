pub mod cycle_detection;
pub mod discrete_math;
pub mod pathfinding;

use num_enum::{IntoPrimitive, TryFromPrimitive};

#[macro_export]
macro_rules! read_input_file {
    () => {{
        let args: Vec<_> = std::env::args().collect();
        if let Some(filename) = args.get(1).map(|s| &s[..]) {
            std::borrow::Cow::from(std::fs::read_to_string(filename).unwrap())
        } else {
            std::borrow::Cow::from(include_str!("../input.txt"))
        }
    }};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, TryFromPrimitive, IntoPrimitive)]
#[repr(usize)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    pub const ALL: [Self; 4] = [Self::North, Self::East, Self::South, Self::West];

    pub const UP: Self = Self::North;
    pub const RIGHT: Self = Self::East;
    pub const DOWN: Self = Self::South;
    pub const LEFT: Self = Self::West;

    pub fn delta(self) -> (isize, isize) {
        match self {
            Direction::North => (0, -1),
            Direction::East => (1, 0),
            Direction::South => (0, 1),
            Direction::West => (-1, 0),
        }
    }

    pub fn step(self, pos: (isize, isize)) -> (isize, isize) {
        let (dx, dy) = self.delta();
        (pos.0 + dx, pos.1 + dy)
    }

    pub fn step_n(self, pos: (isize, isize), n: isize) -> (isize, isize) {
        let (dx, dy) = self.delta();
        (pos.0 + dx * n, pos.1 + dy * n)
    }

    pub fn rotate_cw(self) -> Self {
        self.rotate_cw_by(1)
    }

    pub fn rotate_ccw(self) -> Self {
        self.rotate_ccw_by(1)
    }

    pub fn rotate_cw_by(self, x: usize) -> Self {
        ((usize::from(self) + x) % 4).try_into().unwrap()
    }

    pub fn rotate_ccw_by(self, x: usize) -> Self {
        self.rotate_cw_by(4 - (x % 4))
    }

    pub fn flip(self) -> Self {
        match self {
            Direction::North => Direction::South,
            Direction::East => Direction::West,
            Direction::South => Direction::North,
            Direction::West => Direction::East,
        }
    }
}
