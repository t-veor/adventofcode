use std::ops::{Index, IndexMut};

use utils::read_input_file;

#[derive(Debug, Clone)]
struct Memo2D<T> {
    width: usize,
    data: Vec<T>,
}

impl<T: Clone> Memo2D<T> {
    fn init(width: usize, height: usize, init: T) -> Self {
        Self {
            width,
            data: vec![init; width * height],
        }
    }
}

impl<T> Index<(usize, usize)> for Memo2D<T> {
    type Output = T;

    fn index(&self, (x, y): (usize, usize)) -> &Self::Output {
        self.data.index(y * self.width + x)
    }
}

impl<T> IndexMut<(usize, usize)> for Memo2D<T> {
    fn index_mut(&mut self, (x, y): (usize, usize)) -> &mut Self::Output {
        self.data.index_mut(y * self.width + x)
    }
}

fn can_be_empty(&c: &u8) -> bool {
    c == b'.' || c == b'?'
}

fn can_be_filled(&c: &u8) -> bool {
    c == b'#' || c == b'?'
}

fn count_arrangements(nonogram: &[u8], groups: &[usize]) -> u64 {
    let mut memo = Memo2D::init(nonogram.len() + 1, groups.len() + 1, 0);

    for n_idx in (0..=nonogram.len()).rev() {
        for g_idx in (0..=groups.len()).rev() {
            if g_idx == groups.len() {
                // Reached end of groups array
                if nonogram[n_idx..].iter().all(can_be_empty) {
                    memo[(n_idx, g_idx)] = 1;
                }
                continue;
            }

            let current_group = groups[g_idx];
            if n_idx + current_group > nonogram.len() {
                // Not enough space to fit the current group
                continue;
            }

            let next_cells_can_be_filled = nonogram[n_idx..n_idx + current_group]
                .iter()
                .all(can_be_filled);
            let boundary_exists_after_group = n_idx + current_group == nonogram.len()
                || can_be_empty(&nonogram[n_idx + current_group]);

            if next_cells_can_be_filled && boundary_exists_after_group {
                // Add possibilities resulting from placing current group here
                memo[(n_idx, g_idx)] +=
                    memo[((n_idx + current_group + 1).min(nonogram.len()), g_idx + 1)];
            }

            if can_be_empty(&nonogram[n_idx]) {
                // Add possibilities resulting from skipping current space and
                // leaving it empty
                memo[(n_idx, g_idx)] += memo[(n_idx + 1, g_idx)];
            }
        }
    }

    memo[(0, 0)]
}

fn parse_input(input: &str) -> Vec<(Vec<u8>, Vec<usize>)> {
    input
        .lines()
        .map(|line| {
            let (nonogram, groups) = line.split_once(' ').unwrap();
            let groups = groups.split(',').map(|i| i.parse().unwrap()).collect();
            (nonogram.as_bytes().to_vec(), groups)
        })
        .collect()
}

fn star1(input: &[(Vec<u8>, Vec<usize>)]) -> u64 {
    input
        .iter()
        .map(|(nonogram, groups)| count_arrangements(nonogram, groups))
        .sum::<u64>()
}

fn star2(input: &[(Vec<u8>, Vec<usize>)]) -> u64 {
    input
        .iter()
        .map(|(nonogram, groups)| {
            let unfolded_nonogram = vec![nonogram.to_vec(); 5].join(&b'?');
            let unfolded_groups = groups.repeat(5);

            count_arrangements(&unfolded_nonogram, &unfolded_groups)
        })
        .sum::<u64>()
}

fn main() {
    let input = read_input_file!();
    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
