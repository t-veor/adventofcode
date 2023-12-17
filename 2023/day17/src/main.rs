use utils::{pathfinding::dijkstra, read_input_file, Direction};

fn parse_input(input: &str) -> Vec<Vec<u8>> {
    input
        .lines()
        .map(|line| line.as_bytes().iter().map(|&c| c - b'0').collect())
        .collect()
}

fn star1(map: &[Vec<u8>]) -> i64 {
    let width = map.first().map(|v| v.len()).unwrap_or(0) as isize;
    let height = map.len() as isize;

    dijkstra(
        (0isize, 0isize, None::<Direction>, 0usize),
        |&(x, y, _, _)| (x, y) == (width - 1, height - 1),
        |&(x, y, prev_dir, spaces_in_dir)| {
            let mut results = Vec::with_capacity(3);

            let possible_moves = if let Some(prev_dir) = prev_dir {
                let mut moves = vec![(prev_dir.rotate_cw(), true), (prev_dir.rotate_ccw(), true)];
                if spaces_in_dir < 3 {
                    moves.push((prev_dir, false));
                }
                moves
            } else {
                Direction::ALL.iter().map(|&dir| (dir, true)).collect()
            };

            for (dir, is_turn) in possible_moves {
                let (nx, ny) = dir.step((x, y));

                if (0..width).contains(&nx) && (0..height).contains(&ny) {
                    let cost = map[ny as usize][nx as usize] as i64;
                    results.push((
                        (
                            nx,
                            ny,
                            Some(dir),
                            if is_turn { 1 } else { spaces_in_dir + 1 },
                        ),
                        cost,
                    ))
                }
            }

            results
        },
    )
    .unwrap()
    .0
}

fn star2(map: &[Vec<u8>]) -> i64 {
    let width = map.first().map(|v| v.len()).unwrap_or(0) as isize;
    let height = map.len() as isize;

    dijkstra(
        (0isize, 0isize, None::<Direction>, 0usize),
        |&(x, y, _, spaces_in_dir)| (x, y) == (width - 1, height - 1) && spaces_in_dir >= 4,
        |&(x, y, prev_dir, spaces_in_dir)| {
            let mut results = Vec::with_capacity(3);

            let possible_moves = if let Some(prev_dir) = prev_dir {
                let mut moves = Vec::with_capacity(3);
                if spaces_in_dir < 10 {
                    moves.push((prev_dir, false));
                }
                if spaces_in_dir >= 4 {
                    moves.extend_from_slice(&[
                        (prev_dir.rotate_cw(), true),
                        (prev_dir.rotate_ccw(), true),
                    ]);
                }
                moves
            } else {
                Direction::ALL.iter().map(|&dir| (dir, true)).collect()
            };

            for (dir, is_turn) in possible_moves {
                let (nx, ny) = dir.step((x, y));

                if (0..width).contains(&nx) && (0..height).contains(&ny) {
                    let cost = map[ny as usize][nx as usize] as i64;
                    results.push((
                        (
                            nx,
                            ny,
                            Some(dir),
                            if is_turn { 1 } else { spaces_in_dir + 1 },
                        ),
                        cost,
                    ))
                }
            }

            results
        },
    )
    .unwrap()
    .0
}

fn main() {
    let input = read_input_file!();
    let map = parse_input(&input);

    println!("{}", star1(&map));
    println!("{}", star2(&map));
}
