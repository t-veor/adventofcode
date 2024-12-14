use std::{collections::HashSet, fmt::Write as _};

use regex::Regex;
use utils::{
    discrete_math::chinese_remainder_theorem,
    glam::{i64vec2, I64Vec2},
    read_input_file,
};

fn simulate_robot(start_pos: I64Vec2, velocity: I64Vec2, bounds: I64Vec2, steps: i64) -> I64Vec2 {
    (start_pos + velocity * steps).rem_euclid(bounds)
}

fn simulate_robots_iter(
    robots: &[(I64Vec2, I64Vec2)],
    bounds: I64Vec2,
    steps: i64,
) -> impl Iterator<Item = I64Vec2> + '_ {
    robots
        .iter()
        .copied()
        .map(move |(pos, vel)| simulate_robot(pos, vel, bounds, steps))
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

    quadrant_counts.iter().product()
}

#[allow(unused)]
fn as_pbm(
    robot_positions: impl Iterator<Item = I64Vec2>,
    bounds: I64Vec2,
) -> Result<String, std::fmt::Error> {
    let robot_positions: HashSet<_> = robot_positions.collect();
    let mut pbm = String::new();

    let (width, height) = bounds.into();

    writeln!(&mut pbm, "P1")?;
    writeln!(&mut pbm, "{width} {height}\n")?;

    for y in 0..height {
        for x in 0..width {
            if robot_positions.contains(&i64vec2(x, y)) {
                write!(&mut pbm, "0")?;
            } else {
                write!(&mut pbm, "1")?;
            }
        }
        writeln!(&mut pbm)?;
    }

    Ok(pbm)
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

    safety_factor(simulate_robots_iter(robots, bounds, steps), bounds)
}

fn variance(values: &[i64]) -> f64 {
    let mean = values.iter().sum::<i64>() as f64 / values.len() as f64;

    values
        .iter()
        .map(|&x| {
            let diff = x as f64 - mean;
            diff * diff
        })
        .sum()
}

fn star2(robots: &[(I64Vec2, I64Vec2)]) -> i64 {
    let bounds = i64vec2(101, 103);

    // The x coordinates of the robots must repeat after 103 steps (since that's the
    // length of the cycle), and the y coordinates must repeat after 101, so we
    // simply need to find where in the 103 cycle the x coordinates are at the start
    // and where in the 101 cycle the y coordinates are. When the cycles
    // coincide is when we'll have the christmas tree image.

    // Then, we can just apply the chinese remainder theorem to find out what
    // the first time the two cycles would coincide.

    // I saw a neat trick where we can use the variance to detect
    // programmatically when the frame all the robots line up on a specific axis
    // is.

    let x_variances: Vec<_> = (0..bounds.x)
        .map(|i| {
            let x_coords: Vec<i64> = simulate_robots_iter(robots, bounds, i)
                .map(|pos| pos.x)
                .collect();
            variance(&x_coords)
        })
        .collect();
    let y_variances: Vec<_> = (0..bounds.x)
        .map(|i| {
            let y_coords: Vec<i64> = simulate_robots_iter(robots, bounds, i)
                .map(|pos| pos.y)
                .collect();
            variance(&y_coords)
        })
        .collect();

    let min_x_variance = x_variances
        .iter()
        .enumerate()
        .min_by(|(_, a), (_, b)| a.total_cmp(b))
        .unwrap()
        .0;
    let min_y_variance = y_variances
        .iter()
        .enumerate()
        .min_by(|(_, a), (_, b)| a.total_cmp(b))
        .unwrap()
        .0;

    chinese_remainder_theorem(
        &[min_x_variance as _, min_y_variance as _],
        &[bounds.x, bounds.y],
    )
    .unwrap()
}

fn main() {
    let input = read_input_file!();
    let robots = parse_input(&input);

    println!("{}", star1(&robots));
    println!("{}", star2(&robots));
}
