#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{collections::HashSet, ops::Add};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct IVec3 {
    x: i64,
    y: i64,
    z: i64,
}

impl IVec3 {
    fn manhattan_to_origin(self) -> i64 {
        self.x.abs() + self.y.abs() + self.z.abs()
    }
}

#[derive(Debug, Clone)]
struct Particle {
    p: IVec3,
    v: IVec3,
    a: IVec3,
}

impl Particle {
    fn transform_all_components_to_eventually_increase(&self) -> Self {
        fn transform_component(p: &mut Particle, accessor: impl Fn(&mut IVec3) -> &mut i64) {
            let a_signum = accessor(&mut p.a).signum();
            if a_signum != 0 {
                *accessor(&mut p.a) *= a_signum;
                *accessor(&mut p.v) *= a_signum;
                *accessor(&mut p.p) *= a_signum;
            } else {
                let v_signum = accessor(&mut p.v).signum();
                if v_signum != 0 {
                    *accessor(&mut p.v) *= v_signum;
                    *accessor(&mut p.p) *= v_signum;
                } else {
                    *accessor(&mut p.p) = accessor(&mut p.p).abs();
                }
            }
        }

        let mut transformed = self.clone();
        transform_component(&mut transformed, |v| &mut v.x);
        transform_component(&mut transformed, |v| &mut v.y);
        transform_component(&mut transformed, |v| &mut v.z);

        transformed
    }

    fn step(&mut self) {
        self.v = self.v + self.a;
        self.p = self.p + self.v;
    }

    fn find_collision(&self, other: &Self) -> Option<i64> {
        fn solve_for_component(
            this: &Particle,
            other: &Particle,
            accessor: impl Fn(&IVec3) -> i64,
        ) -> QuadraticResult {
            let a = accessor(&this.a) - accessor(&other.a);
            let b = 2 * (accessor(&this.v) - accessor(&other.v)) + a;
            let c = 2 * (accessor(&this.p) - accessor(&other.p));

            solve_integer_quadratic(a, b, c)
        }

        let results = [
            solve_for_component(self, other, |v| v.x),
            solve_for_component(self, other, |v| v.y),
            solve_for_component(self, other, |v| v.z),
        ];

        let mut possible_ts: Option<HashSet<i64>> = None;
        for result in results {
            if let Some(set) = result.as_set() {
                if let Some(existing) = possible_ts {
                    possible_ts = Some(existing.intersection(&set).copied().collect());
                } else {
                    possible_ts = Some(set);
                }
            }
        }

        match possible_ts {
            Some(set) => set.into_iter().filter(|&i| i >= 0).min(),
            None => Some(0),
        }
    }
}

impl Add<IVec3> for IVec3 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        IVec3 {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum QuadraticResult {
    NoSolutions,
    OneSolution(i64),
    TwoSolutions(i64, i64),
    EntireNumberLine,
}

impl QuadraticResult {
    fn as_set(self) -> Option<HashSet<i64>> {
        match self {
            QuadraticResult::NoSolutions => Some(HashSet::new()),
            QuadraticResult::OneSolution(x) => Some(HashSet::from([x])),
            QuadraticResult::TwoSolutions(x, y) => Some(HashSet::from([x, y])),
            QuadraticResult::EntireNumberLine => None,
        }
    }
}

fn solve_integer_quadratic(a: i64, b: i64, c: i64) -> QuadraticResult {
    if a == 0 {
        if b == 0 {
            // edge case: c = 0
            return if c == 0 {
                QuadraticResult::EntireNumberLine
            } else {
                QuadraticResult::NoSolutions
            };
        }

        // edge case: linear equation
        return if c % b == 0 {
            QuadraticResult::OneSolution(-c / b)
        } else {
            QuadraticResult::NoSolutions
        };
    }

    let b_sq = b * b;
    let discrim_sq = b_sq - 4 * a * c;
    if discrim_sq.is_negative() {
        return QuadraticResult::NoSolutions;
    }

    let discrim = (discrim_sq as f64).sqrt() as i64;
    if discrim * discrim != discrim_sq {
        // No integer solutions
        return QuadraticResult::NoSolutions;
    }

    let num_x = -b - discrim;
    let num_y = -b + discrim;
    let denom = 2 * a;

    if num_x % denom == 0 && num_y % denom == 0 {
        QuadraticResult::TwoSolutions(num_x / denom, num_y / denom)
    } else if num_x % denom == 0 {
        QuadraticResult::OneSolution(num_x / denom)
    } else if num_y % denom == 0 {
        QuadraticResult::OneSolution(num_y / denom)
    } else {
        QuadraticResult::NoSolutions
    }
}

fn parse_input(input: &str) -> Vec<Particle> {
    fn parse_ivec3(v: &str) -> IVec3 {
        let pat: &[_] = &['<', '>'];
        let v = v.trim_matches(pat);
        let components = v.split(',').collect::<Vec<_>>();
        let x = components[0].parse().unwrap();
        let y = components[1].parse().unwrap();
        let z = components[2].parse().unwrap();

        IVec3 { x, y, z }
    }

    fn parse_line(line: &str) -> Particle {
        let components = line.trim().split(", ").collect::<Vec<_>>();
        let p = parse_ivec3(components[0].trim_start_matches("p="));
        let v = parse_ivec3(components[1].trim_start_matches("v="));
        let a = parse_ivec3(components[2].trim_start_matches("a="));

        Particle { p, v, a }
    }

    input.lines().map(parse_line).collect()
}

fn star1(input: &[Particle]) -> usize {
    input
        .iter()
        .enumerate()
        .min_by_key(|(_, p)| {
            let p = p.transform_all_components_to_eventually_increase();
            (
                p.a.manhattan_to_origin(),
                p.v.manhattan_to_origin(),
                p.p.manhattan_to_origin(),
            )
        })
        .unwrap()
        .0
}

fn star2(input: &[Particle]) -> usize {
    let mut collisions = Vec::new();
    for i in 0..input.len() {
        for j in i + 1..input.len() {
            if let Some(collide_time) = input[i].find_collision(&input[j]) {
                collisions.push((collide_time, i, j));
            }
        }
    }

    collisions.sort();

    let mut collisions_grouped_by_t: Vec<Vec<_>> = Vec::new();
    let mut last_t = None;

    for collision in collisions {
        if Some(collision.0) == last_t {
            collisions_grouped_by_t.last_mut().unwrap().push(collision);
        } else {
            last_t = Some(collision.0);
            collisions_grouped_by_t.push(vec![collision]);
        }
    }

    let mut still_alive: HashSet<_> = (0..input.len()).collect();
    let mut to_remove = Vec::new();

    for group in collisions_grouped_by_t {
        for (_, i, j) in group {
            if still_alive.contains(&i) && still_alive.contains(&j) {
                to_remove.push(i);
                to_remove.push(j);
            }
        }

        for i in to_remove.drain(..) {
            still_alive.remove(&i);
        }
    }

    still_alive.len()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
