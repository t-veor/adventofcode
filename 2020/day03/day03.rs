#!/usr/bin/env run-cargo-script

fn count_trees(grid: &Vec<Vec<bool>>, angle: (usize, usize)) -> usize {
    let mut count = 0;
    let (mut x, mut y) = (0, 0);
    let (dx, dy) = angle;
    while y < grid.len() {
        let row = &grid[y];
        if row[x % row.len()] {
            count += 1;
        }
        x += dx;
        y += dy;
    }

    count
}

fn star1(input: &Vec<Vec<bool>>) -> usize {
    count_trees(input, (3, 1))
}

fn star2(input: &Vec<Vec<bool>>) -> usize {
    let angles = &[(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
    angles
        .iter()
        .map(|i| count_trees(input, *i))
        .fold(1, |x, y| x * y)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename)
        .unwrap()
        .lines()
        .map(|l| l.chars().map(|c| c == '#').collect())
        .collect();

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
