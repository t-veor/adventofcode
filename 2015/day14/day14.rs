#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

#[derive(Debug, Clone)]
struct Reindeer {
    speed: i32,
    flight_time: i32,
    rest_time: i32,
}

fn parse_input(input: String) -> Vec<Reindeer> {
    fn extract(line: &str) -> (&str, Reindeer) {
        let split: Vec<_> = line.split_ascii_whitespace().collect();
        let name = split[0];
        let speed = split[3].parse().unwrap();
        let flight_time = split[6].parse().unwrap();
        let rest_time = split[13].parse().unwrap();

        (
            name,
            Reindeer {
                speed,
                flight_time,
                rest_time,
            },
        )
    }

    input.lines().map(|line| extract(line).1).collect()
}

#[derive(Debug)]
struct RaceState {
    curr_time: i32,
    state: Vec<ReindeerState>,
}

#[derive(Debug)]
struct ReindeerState {
    speed: i32,
    flight_time: i32,
    rest_time: i32,

    position: i32,
    is_resting: bool,
    toggle_rest_time: i32,
}

impl RaceState {
    fn new(reindeer: &[Reindeer]) -> Self {
        Self {
            curr_time: 0,
            state: reindeer
                .iter()
                .map(|reindeer| ReindeerState {
                    speed: reindeer.speed,
                    flight_time: reindeer.flight_time,
                    rest_time: reindeer.rest_time,
                    position: 0,
                    is_resting: false,
                    toggle_rest_time: reindeer.flight_time,
                })
                .collect(),
        }
    }

    fn curr_time(&self) -> i32 {
        self.curr_time
    }

    fn step(&mut self) {
        self.curr_time += 1;

        for reindeer in self.state.iter_mut() {
            if !reindeer.is_resting {
                reindeer.position += reindeer.speed;
            }

            if reindeer.toggle_rest_time == self.curr_time {
                reindeer.is_resting = !reindeer.is_resting;
                reindeer.toggle_rest_time += if reindeer.is_resting {
                    reindeer.rest_time
                } else {
                    reindeer.flight_time
                };
            }
        }
    }

    fn furthest(&self) -> i32 {
        self.state
            .iter()
            .map(|reindeer| reindeer.position)
            .max()
            .unwrap()
    }

    fn furthest_reindeer(&self) -> impl Iterator<Item = usize> + '_ {
        let furthest = self.furthest();
        self.state
            .iter()
            .enumerate()
            .filter_map(move |(i, reindeer)| {
                if reindeer.position == furthest {
                    Some(i)
                } else {
                    None
                }
            })
    }
}

fn star1(input: &[Reindeer]) -> i32 {
    let mut race = RaceState::new(input);

    while race.curr_time() <= 2503 {
        race.step();
    }

    race.furthest()
}

fn star2(input: &[Reindeer]) -> i32 {
    let mut race = RaceState::new(input);
    let mut points = vec![0; input.len()];

    while race.curr_time() <= 2503 {
        race.step();

        for i in race.furthest_reindeer() {
            points[i] += 1;
        }
    }

    points.into_iter().max().unwrap()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
