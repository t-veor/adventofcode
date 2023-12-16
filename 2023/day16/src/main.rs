use std::collections::HashSet;

use utils::read_input_file;

#[derive(Debug, Clone)]
struct Contraption {
    layout: Vec<u8>,
    size: (usize, usize),
}

impl Contraption {
    fn parse_input(input: &str) -> Self {
        let lines: Vec<_> = input.lines().collect();
        let width = lines.first().unwrap().len();
        let height = lines.len();

        let layout: Vec<_> = lines
            .into_iter()
            .flat_map(|line| line.as_bytes().iter().copied())
            .collect();
        assert_eq!(layout.len(), width * height);

        Self {
            layout,
            size: (width, height),
        }
    }

    fn to_index(&self, (x, y): (isize, isize)) -> Option<usize> {
        if x < 0 || y < 0 {
            return None;
        }
        let (x, y) = (x as usize, y as usize);
        let (width, height) = self.size;

        if x >= width || y >= height {
            None
        } else {
            Some(y * width + x)
        }
    }

    fn in_bounds(&self, pos: (isize, isize)) -> bool {
        self.to_index(pos).is_some()
    }

    fn get(&self, pos: (isize, isize)) -> Option<&u8> {
        self.layout.get(self.to_index(pos)?)
    }

    fn get_next_dirs(&self, pos: (isize, isize), dir: (isize, isize)) -> Vec<(isize, isize)> {
        let tile = match self.get(pos) {
            Some(tile) => *tile,
            None => return vec![],
        };

        let (dx, dy) = dir;

        match tile {
            b'/' => vec![(-dy, -dx)],
            b'\\' => vec![(dy, dx)],
            b'-' if dx == 0 => vec![(1, 0), (-1, 0)],
            b'|' if dy == 0 => vec![(0, 1), (0, -1)],
            _ => vec![dir],
        }
    }
}

fn energized_tiles(
    contraption: &Contraption,
    initial_pos: (isize, isize),
    initial_dir: (isize, isize),
) -> usize {
    let mut seen = HashSet::new();
    let mut queue = Vec::new();

    seen.insert((initial_pos, initial_dir));
    queue.push((initial_pos, initial_dir));

    while let Some((pos, dir)) = queue.pop() {
        for next_dir in contraption.get_next_dirs(pos, dir) {
            let next_pos = (pos.0 + next_dir.0, pos.1 + next_dir.1);
            if contraption.in_bounds(next_pos) && seen.insert((next_pos, next_dir)) {
                queue.push((next_pos, next_dir));
            }
        }
    }

    seen.into_iter()
        .map(|(pos, _)| pos)
        .collect::<HashSet<_>>()
        .len()
}

fn star1(contraption: &Contraption) -> usize {
    energized_tiles(contraption, (0, 0), (1, 0))
}

fn star2(contraption: &Contraption) -> usize {
    let (width, height) = (contraption.size.0 as isize, contraption.size.1 as isize);
    let top_entries = (0..width).map(|x| ((x, 0), (0, 1)));
    let bottom_entries = (0..width).map(|x| ((x, height - 1), (0, -1)));
    let left_entries = (0..height).map(|y| ((0, y), (1, 0)));
    let right_entries = (0..height).map(|y| ((width - 1, y), (-1, 0)));

    top_entries
        .chain(bottom_entries)
        .chain(left_entries)
        .chain(right_entries)
        .map(|(pos, dir)| energized_tiles(contraption, pos, dir))
        .max()
        .unwrap()
}

fn main() {
    let input = read_input_file!();
    let contraption = Contraption::parse_input(&input);

    println!("{}", star1(&contraption));
    println!("{}", star2(&contraption));
}
