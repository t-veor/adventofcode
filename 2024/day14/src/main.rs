use regex::Regex;
use utils::{
    glam::{i64vec2, I64Vec2},
    read_input_file,
};

fn simulate_robot(start_pos: I64Vec2, velocity: I64Vec2, bounds: I64Vec2, steps: i64) -> I64Vec2 {
    (start_pos + velocity * steps).rem_euclid(bounds)
}

fn safety_factor(robot_positions: impl Iterator<Item = I64Vec2>, bounds: I64Vec2) -> i64 {
    assert!(bounds.x % 2 == 1 && bounds.y % 2 == 1);

    let mut quadrant_counts = [0; 4];

    for robot in robot_positions {
        if robot.x == bounds.x / 2 || robot.y == bounds.y / 2 {
            continue;
        }

        let in_left_quadrant = robot.x < bounds.x / 2;
        let in_top_quadrant = robot.y < bounds.y / 2;

        let quadrant_idx = match (in_left_quadrant, in_top_quadrant) {
            (true, true) => 0,
            (true, false) => 1,
            (false, true) => 2,
            (false, false) => 3,
        };
        quadrant_counts[quadrant_idx] += 1;
    }

    dbg!(&quadrant_counts);

    quadrant_counts.iter().product()
}

fn parse_input(input: &str) -> Vec<(I64Vec2, I64Vec2)> {
    let pat = Regex::new(r"[-+]?\d+").unwrap();

    input
        .lines()
        .map(|line| {
            let numbers: Vec<_> = pat
                .find_iter(line)
                .map(|m| m.as_str().parse().unwrap())
                .collect();
            let [px, py, vx, vy] = numbers[..] else {
                unreachable!()
            };

            (i64vec2(px, py), i64vec2(vx, vy))
        })
        .collect()
}

fn star1(robots: &[(I64Vec2, I64Vec2)]) -> i64 {
    let bounds = i64vec2(101, 103);
    let steps = 100;

    safety_factor(
        robots
            .iter()
            .copied()
            .map(|(pos, vel)| simulate_robot(pos, vel, bounds, steps)),
        bounds,
    )
}

fn main() {
    let input = read_input_file!();
    let robots = parse_input(&input);

    println!("{}", star1(&robots));
}
