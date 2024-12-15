use std::hash::Hash;
use std::ops::IndexMut;
use std::{fmt::Debug, ops::Index};

use glam::{ivec2, IVec2};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use thiserror::Error;

#[derive(Clone)]
pub struct DenseGrid<T> {
    grid: Vec<T>,
    size: (i32, i32),
}

impl<T> DenseGrid<T> {
    pub fn new(size: (i32, i32), mut init: impl FnMut(IVec2) -> T) -> Self {
        let (width, height) = size;
        assert!(
            width >= 0 && height >= 0,
            "Can't make a grid with negative size!"
        );

        let len = width * height;
        let mut grid = Vec::with_capacity(len as usize);

        for i in 0..len {
            let y = i / width;
            let x = i % height;
            grid.push(init(ivec2(x as _, y as _)));
        }

        Self { grid, size }
    }

    pub fn read_from_str(input: &str, mut map: impl FnMut(IVec2, char) -> T) -> Option<Self> {
        let mut width = None;
        let mut height = 0;

        let mut grid = Vec::new();

        for (y, line) in input.trim().lines().enumerate() {
            height += 1;

            let mut this_line_width = 0;
            for (x, char) in line.chars().enumerate() {
                grid.push(map(ivec2(x as _, y as _), char));
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

    pub fn size(&self) -> (i32, i32) {
        self.size
    }

    pub fn width(&self) -> i32 {
        self.size.0
    }

    pub fn height(&self) -> i32 {
        self.size.1
    }

    pub fn index_from_coords(&self, coords: IVec2) -> Option<usize> {
        if (0..self.width()).contains(&coords.x) && (0..self.height()).contains(&coords.y) {
            Some((coords.y * self.width() + coords.x) as usize)
        } else {
            None
        }
    }

    pub fn in_bounds(&self, coords: IVec2) -> bool {
        self.index_from_coords(coords).is_some()
    }

    pub fn coords_from_index(&self, index: usize) -> Option<IVec2> {
        if index < self.grid.len() {
            let y = index / self.width() as usize;
            let x = index % self.width() as usize;
            Some(ivec2(x as _, y as _))
        } else {
            None
        }
    }

    pub fn get(&self, coords: IVec2) -> Option<&T> {
        let index = self.index_from_coords(coords)?;
        self.grid.get(index)
    }

    pub fn get_mut(&mut self, coords: IVec2) -> Option<&mut T> {
        let index = self.index_from_coords(coords)?;
        self.grid.get_mut(index)
    }

    pub fn coord_iter(&self) -> impl Iterator<Item = IVec2> {
        let (width, height) = self.size;
        (0..height).flat_map(move |y| (0..width).map(move |x| ivec2(x, y)))
    }

    pub fn von_neumann_neighbors(&self, pos: IVec2) -> Vec<IVec2> {
        von_neumann_neighbors(pos)
            .into_iter()
            .filter(|&pos| self.in_bounds(pos))
            .collect()
    }

    pub fn moore_neighbors(&self, pos: IVec2) -> Vec<IVec2> {
        moore_neighbors(pos)
            .into_iter()
            .filter(|&pos| self.in_bounds(pos))
            .collect()
    }
}

impl<T: PartialEq> DenseGrid<T> {
    pub fn find(&self, target: &T) -> Option<IVec2> {
        self.grid
            .iter()
            .position(|c| c == target)
            .and_then(|idx| self.coords_from_index(idx))
    }
}

pub type CharGrid = DenseGrid<char>;

impl CharGrid {
    pub fn from_str_chars(input: &str) -> Option<Self> {
        Self::read_from_str(input, |_, c| c)
    }
}

impl<T> Index<IVec2> for DenseGrid<T> {
    type Output = T;

    fn index(&self, index: IVec2) -> &Self::Output {
        self.get(index).expect("Grid coords out of bounds")
    }
}

impl<T> IndexMut<IVec2> for DenseGrid<T> {
    fn index_mut(&mut self, index: IVec2) -> &mut Self::Output {
        self.get_mut(index).expect("Grid coords out of bounds")
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

    pub fn delta(self) -> IVec2 {
        match self {
            OrthoDir::North => ivec2(0, -1),
            OrthoDir::East => ivec2(1, 0),
            OrthoDir::South => ivec2(0, 1),
            OrthoDir::West => ivec2(-1, 0),
        }
    }

    pub fn step(self, pos: IVec2) -> IVec2 {
        pos + self.delta()
    }

    pub fn step_n(self, pos: IVec2, n: i32) -> IVec2 {
        pos + n * self.delta()
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

    pub fn as_index(self) -> usize {
        u8::from(self) as usize
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

    pub fn delta(self) -> IVec2 {
        match self {
            DiagDir::North => ivec2(0, -1),
            DiagDir::NorthEast => ivec2(1, -1),
            DiagDir::East => ivec2(1, 0),
            DiagDir::SouthEast => ivec2(1, 1),
            DiagDir::South => ivec2(0, 1),
            DiagDir::SouthWest => ivec2(-1, 1),
            DiagDir::West => ivec2(-1, 0),
            DiagDir::NorthWest => ivec2(-1, -1),
        }
    }

    pub fn step(self, pos: IVec2) -> IVec2 {
        pos + self.delta()
    }

    pub fn step_n(self, pos: IVec2, n: i32) -> IVec2 {
        pos + n * self.delta()
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

    pub fn as_index(self) -> usize {
        u8::from(self) as usize
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

pub fn von_neumann_neighbors(pos: IVec2) -> [IVec2; 4] {
    OrthoDir::ALL.map(|dir| dir.step(pos))
}

pub fn moore_neighbors(pos: IVec2) -> [IVec2; 8] {
    DiagDir::ALL.map(|dir| dir.step(pos))
}
