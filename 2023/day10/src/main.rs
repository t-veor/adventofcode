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
    start_pos: (isize, isize),
    start_connections: Dir,
}

impl PipeGrid {
    fn parse(input: &str) -> Self {
        let grid: Vec<_> = input.lines().map(|line| line.as_bytes().to_vec()).collect();

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
}

fn find_path(grid: &PipeGrid) -> Vec<(isize, isize)> {
    let mut path = vec![grid.start_pos];

    let mut next_dir = grid.start_connections.pick();
    let mut pos = grid.start_pos;
    loop {
        let delta = next_dir.delta().unwrap();
        pos = (pos.0 + delta.0, pos.1 + delta.1);

        let connections = grid.connections_at(pos);
        next_dir = connections.difference(next_dir.flip());

        if pos == grid.start_pos {
            break;
        }

        path.push(pos);
    }

    path
}

fn star1(grid: &PipeGrid) -> usize {
    find_path(grid).len() / 2
}

fn shoelace_area(path: &[(isize, isize)]) -> isize {
    (0..path.len())
        .map(|i| (path[i], path[(i + 1) % path.len()]))
        .map(|((x0, y0), (x1, y1))| x0 * y1 - x1 * y0)
        .sum::<isize>()
        .abs()
        / 2
}

fn star2(grid: &PipeGrid) -> isize {
    let path = find_path(grid);
    let boundary_points = path.len() as isize;
    let enclosed_area = shoelace_area(&path);

    // Pick's theorem
    enclosed_area + 1 - boundary_points / 2
}

fn main() {
    let input = read_input_file!();
    let grid = PipeGrid::parse(&input);

    println!("{}", star1(&grid));
    println!("{}", star2(&grid));
}
