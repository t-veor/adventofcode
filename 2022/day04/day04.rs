#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Assignment(i32, i32);

impl Assignment {
    fn intersect(self, other: Self) -> Self {
        Self(self.0.max(other.0), self.1.min(other.1))
    }

    fn is_valid(self) -> bool {
        self.0 <= self.1
    }
}

fn parse_assignment(assignment: &str) -> Assignment {
    let assignment: Vec<_> = assignment.split('-').collect();
    Assignment(
        assignment[0].parse().unwrap(),
        assignment[1].parse().unwrap(),
    )
}

fn parse_assignment_pair(pair: &str) -> (Assignment, Assignment) {
    let pair: Vec<_> = pair.split(',').collect();
    (parse_assignment(pair[0]), parse_assignment(pair[1]))
}

fn parse_input(input: String) -> Vec<(Assignment, Assignment)> {
    input.lines().map(parse_assignment_pair).collect()
}

fn star1(input: &[(Assignment, Assignment)]) -> usize {
    input
        .iter()
        .filter(|(a, b)| {
            let c = a.intersect(*b);
            *a == c || *b == c
        })
        .count()
}

fn star2(input: &[(Assignment, Assignment)]) -> usize {
    input
        .iter()
        .filter(|(a, b)| a.intersect(*b).is_valid())
        .count()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
