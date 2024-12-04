use std::fmt::Debug;
use std::hash::Hash;

use num_enum::{IntoPrimitive, TryFromPrimitive};
use thiserror::Error;

#[derive(Clone)]
pub struct CharGrid {
    grid: Vec<char>,
    size: (usize, usize),
}

impl CharGrid {
    pub fn from_input_str(input: &str) -> Option<Self> {
        let mut width = None;
        let mut height = 0;

        let mut grid = Vec::new();

        for line in input.trim().lines() {
            height += 1;

            let mut this_line_width = 0;
            for char in line.chars() {
                grid.push(char);
                this_line_width += 1;
            }

            if let Some(prev_width) = width {
                if prev_width != this_line_width {
                    // Mismatching line widths
                    return None;
                }
            } else {
                width = Some(this_line_width);
            }
        }

        Some(Self {
            grid,
            size: (width?, height),
        })
    }

    pub fn width(&self) -> usize {
        self.size.0
    }

    pub fn height(&self) -> usize {
        self.size.1
    }

    pub fn index_from_coords(&self, coords: (isize, isize)) -> Option<usize> {
        let (x, y) = coords;
        if (0..self.width() as isize).contains(&x) && (0..self.height() as isize).contains(&y) {
            Some((y * self.width() as isize + x) as usize)
        } else {
            None
        }
    }

    pub fn get(&self, coords: (isize, isize)) -> Option<char> {
        let index = self.index_from_coords(coords)?;
        self.grid.get(index).copied()
    }

    pub fn get_mut(&mut self, coords: (isize, isize)) -> Option<&mut char> {
        let index = self.index_from_coords(coords)?;
        self.grid.get_mut(index)
    }

    pub fn coord_iter(&self) -> impl Iterator<Item = (isize, isize)> {
        let (width, height) = self.size;
        (0..height).flat_map(move |y| (0..width).map(move |x| (x as isize, y as isize)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum OrthoDir {
    North,
    East,
    South,
    West,
}

impl OrthoDir {
    pub const ALL: [Self; 4] = [Self::North, Self::East, Self::South, Self::West];

    pub const UP: Self = Self::North;
    pub const RIGHT: Self = Self::East;
    pub const DOWN: Self = Self::South;
    pub const LEFT: Self = Self::West;

    pub fn delta(self) -> (isize, isize) {
        match self {
            OrthoDir::North => (0, -1),
            OrthoDir::East => (1, 0),
            OrthoDir::South => (0, 1),
            OrthoDir::West => (-1, 0),
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

    pub fn rotate_cw_by(self, x: u8) -> Self {
        ((u8::from(self) + x) % 4).try_into().unwrap()
    }

    pub fn rotate_ccw_by(self, x: u8) -> Self {
        self.rotate_cw_by(4 - (x % 4))
    }

    pub fn flip(self) -> Self {
        self.rotate_cw_by(2)
    }

    pub fn as_diag(self) -> DiagDir {
        let raw: u8 = self.into();
        DiagDir::try_from_primitive(raw * 2).unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum DiagDir {
    North,
    NorthEast,
    East,
    SouthEast,
    South,
    SouthWest,
    West,
    NorthWest,
}

impl DiagDir {
    pub const ALL: [Self; 8] = [
        Self::North,
        Self::NorthEast,
        Self::East,
        Self::SouthEast,
        Self::South,
        Self::SouthWest,
        Self::West,
        Self::NorthWest,
    ];

    pub const UP: Self = Self::North;
    pub const RIGHT: Self = Self::East;
    pub const DOWN: Self = Self::South;
    pub const LEFT: Self = Self::West;

    pub fn delta(self) -> (isize, isize) {
        match self {
            DiagDir::North => (0, -1),
            DiagDir::NorthEast => (1, -1),
            DiagDir::East => (1, 0),
            DiagDir::SouthEast => (1, 1),
            DiagDir::South => (0, 1),
            DiagDir::SouthWest => (-1, 1),
            DiagDir::West => (-1, 0),
            DiagDir::NorthWest => (-1, -1),
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

    pub fn rotate_cw_by(self, x: u8) -> Self {
        ((u8::from(self) + x) % 8).try_into().unwrap()
    }

    pub fn rotate_ccw_by(self, x: u8) -> Self {
        self.rotate_cw_by(8 - (x % 8))
    }

    pub fn flip(self) -> Self {
        self.rotate_cw_by(4)
    }

    pub fn try_as_ortho(self) -> Option<OrthoDir> {
        let raw: u8 = self.into();
        if raw % 2 == 0 {
            Some(OrthoDir::try_from_primitive(raw / 2).unwrap())
        } else {
            None
        }
    }
}

impl From<OrthoDir> for DiagDir {
    fn from(value: OrthoDir) -> Self {
        value.as_diag()
    }
}

#[derive(Error, Debug)]
#[error("Could not convert DiagDir to OrthoDir; dir was a diagonal direction")]
pub struct DiagDirConversionError;

impl TryFrom<DiagDir> for OrthoDir {
    type Error = DiagDirConversionError;

    fn try_from(value: DiagDir) -> Result<Self, Self::Error> {
        value.try_as_ortho().ok_or(DiagDirConversionError)
    }
}
