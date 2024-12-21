use std::collections::HashMap;

use utils::{
    glam::{ivec2, IVec2},
    grid::OrthoDir,
    read_input_file,
};

type Pad = &'static [(char, (i32, i32))];

const KEYPAD: Pad = &[
    ('7', (-2, -3)),
    ('8', (-1, -3)),
    ('9', (0, -3)),
    ('4', (-2, -2)),
    ('5', (-1, -2)),
    ('6', (0, -2)),
    ('1', (-2, -1)),
    ('2', (-1, -1)),
    ('3', (0, -1)),
    ('0', (-1, 0)),
    ('A', (0, 0)),
];

const DPAD: Pad = &[
    ('^', (-1, 0)),
    ('A', (0, 0)),
    ('<', (-2, 1)),
    ('v', (-1, 1)),
    ('>', (0, 1)),
];

fn button_pos(button: char, pad: Pad) -> IVec2 {
    pad.iter()
        .find_map(|&(c, v)| {
            if c == button {
                Some(ivec2(v.0, v.1))
            } else {
                None
            }
        })
        .expect("Button does not exist")
}

fn button_at(pos: IVec2, pad: Pad) -> Option<char> {
    let pos = (pos.x, pos.y);
    pad.iter()
        .find_map(|&(c, v)| if v == pos { Some(c) } else { None })
}

fn robot_allowed_movements(
    pos: IVec2,
    target_pos: IVec2,
    is_gap: impl Fn(IVec2) -> bool,
) -> Vec<char> {
    let mut allowed = Vec::with_capacity(2);

    if pos.x < target_pos.x && !is_gap(OrthoDir::RIGHT.step(pos)) {
        allowed.push('>');
    }

    if pos.x > target_pos.x && !is_gap(OrthoDir::LEFT.step(pos)) {
        allowed.push('<');
    }

    if pos.y < target_pos.y && !is_gap(OrthoDir::DOWN.step(pos)) {
        allowed.push('v');
    }

    if pos.y > target_pos.y && !is_gap(OrthoDir::UP.step(pos)) {
        allowed.push('^');
    }

    if pos == target_pos {
        allowed.push('A');
    }

    assert!(!allowed.is_empty());

    allowed
}

fn all_shortest_paths(
    pos: IVec2,
    target_pos: IVec2,
    is_gap: impl Fn(IVec2) -> bool,
) -> Vec<Vec<char>> {
    fn dfs(
        pos: IVec2,
        target_pos: IVec2,
        is_gap: &impl Fn(IVec2) -> bool,
        curr_path: &mut Vec<char>,
        paths: &mut Vec<Vec<char>>,
    ) {
        let allowed = robot_allowed_movements(pos, target_pos, is_gap);
        for char in allowed {
            curr_path.push(char);

            if char == 'A' {
                paths.push(curr_path.clone());
            } else {
                let next_pos = match char {
                    '^' => OrthoDir::UP.step(pos),
                    '>' => OrthoDir::RIGHT.step(pos),
                    'v' => OrthoDir::DOWN.step(pos),
                    '<' => OrthoDir::LEFT.step(pos),
                    _ => unreachable!(),
                };
                dfs(next_pos, target_pos, is_gap, curr_path, paths);
            }

            curr_path.pop();
        }
    }

    let mut paths = Vec::new();
    dfs(pos, target_pos, &is_gap, &mut Vec::new(), &mut paths);
    paths
}

fn shortest_button_sequence(sequence: &[char], depth: usize) -> u64 {
    fn score_sequence(
        sequence: &[char],
        depth: usize,
        is_keypad: bool,
        cache: &mut HashMap<((char, char), usize), u64>,
    ) -> u64 {
        let mut total = 0;

        for i in 0..sequence.len() {
            let a = if i == 0 { 'A' } else { sequence[i - 1] };
            let b = sequence[i];

            total += shortest_for_pair((a, b), depth, is_keypad, cache);
        }

        total
    }

    fn shortest_for_pair(
        pair: (char, char),
        depth: usize,
        is_keypad: bool,
        cache: &mut HashMap<((char, char), usize), u64>,
    ) -> u64 {
        if depth == 0 {
            return 1;
        }

        if let Some(&result) = cache.get(&(pair, depth)) {
            return result;
        }

        let pad = if is_keypad { KEYPAD } else { DPAD };
        let sequences = all_shortest_paths(button_pos(pair.0, pad), button_pos(pair.1, pad), |v| {
            button_at(v, pad).is_none()
        });

        let result = sequences
            .iter()
            .map(|sequence| score_sequence(sequence, depth - 1, false, cache))
            .min()
            .unwrap();

        cache.insert((pair, depth), result);

        result
    }

    score_sequence(sequence, depth, true, &mut HashMap::new())
}

fn code_complexity(code: &str, dpad_robots: usize) -> u64 {
    let buttons = shortest_button_sequence(&code.chars().collect::<Vec<_>>(), dpad_robots + 1);
    let numeric_part: u64 = code.strip_suffix('A').unwrap().parse().unwrap();

    buttons * numeric_part
}

fn parse_input(input: &str) -> Vec<&str> {
    input.trim().lines().collect()
}

fn star1(codes: &[&str]) -> u64 {
    codes.iter().map(|code| code_complexity(code, 2)).sum()
}

fn star2(codes: &[&str]) -> u64 {
    codes.iter().map(|code| code_complexity(code, 25)).sum()
}

fn main() {
    let input = read_input_file!();
    let codes = parse_input(&input);

    println!("{}", star1(&codes));
    println!("{}", star2(&codes));
}
