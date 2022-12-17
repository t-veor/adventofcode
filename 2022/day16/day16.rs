#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! regex = "1"
//! priority-queue = "1.3.0"
//! ```

// Well this went kinda badly. I opted for an A* search over the state space but
// tiny bugs in the heuristics and move generation kept messing things up

use priority_queue::PriorityQueue;
use regex::Regex;
use std::{collections::HashMap, fmt::Debug, hash::Hash};

#[derive(Debug, Default)]
struct ValveGraph {
    index: HashMap<String, usize>,
    flow_rates: Vec<i32>,
    adjacency: Vec<Vec<usize>>,
}

#[derive(Debug, Default)]
struct CompleteValveGraph {
    index: HashMap<String, usize>,
    flow_rates: Vec<i32>,
    dist: Vec<i32>,
    non_zero_valves: Vec<usize>,
}

impl CompleteValveGraph {
    fn from_valve_graph(valves: &ValveGraph) -> Self {
        let node_count = valves.adjacency.len();
        let mut dist = vec![i32::MAX; node_count * node_count];

        for (i, adjacent) in valves.adjacency.iter().enumerate() {
            dist[i * node_count + i] = 0;
            for j in adjacent {
                dist[i * node_count + j] = 1;
            }
        }

        for k in 0..node_count {
            for i in 0..node_count {
                for j in 0..node_count {
                    if dist[i * node_count + j]
                        > dist[i * node_count + k].saturating_add(dist[k * node_count + j])
                    {
                        dist[i * node_count + j] =
                            dist[i * node_count + k].saturating_add(dist[k * node_count + j]);
                    }
                }
            }
        }

        let non_zero_valves = valves
            .flow_rates
            .iter()
            .enumerate()
            .filter_map(|(i, flow_rate)| if *flow_rate > 0 { Some(i) } else { None })
            .collect();

        Self {
            index: valves.index.clone(),
            flow_rates: valves.flow_rates.clone(),
            dist,
            non_zero_valves,
        }
    }

    fn get_dist(&self, from: usize, to: usize) -> i32 {
        let node_count = self.flow_rates.len();
        let idx = from * node_count + to;
        self.dist[idx]
    }
}

fn parse_input(input: String) -> CompleteValveGraph {
    let re =
        Regex::new(r"^Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)$").unwrap();

    let mut flow_rates = Vec::new();
    let mut adjacency = Vec::new();
    let mut index = HashMap::new();

    for line in input.lines() {
        let captures = re.captures(line).unwrap();
        let valve = &captures[1];
        let flow_rate = captures[2].parse().unwrap();
        let adjacent_valves: Vec<_> = captures[3].split(", ").map(|s| s.to_owned()).collect();

        index.insert(valve.to_owned(), adjacency.len());
        flow_rates.push(flow_rate);
        adjacency.push(adjacent_valves);
    }

    CompleteValveGraph::from_valve_graph(&ValveGraph {
        flow_rates,
        adjacency: adjacency
            .into_iter()
            .map(|valves| valves.into_iter().map(|valve| index[&valve]).collect())
            .collect(),
        index,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ValveOpenness(u64);

impl ValveOpenness {
    fn new(valves: &CompleteValveGraph) -> Self {
        assert!(valves.flow_rates.len() < 64);
        Self(0)
    }

    fn is_open(&self, valve: usize) -> bool {
        self.0 & 1 << valve > 0
    }

    fn is_closed(&self, valve: usize) -> bool {
        !self.is_open(valve)
    }

    fn set_open(&self, valve: usize) -> Self {
        Self(self.0 | 1 << valve)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MoveState {
    position: usize,
    time: i32,
    curr_flow: i32,
    valve_openness: ValveOpenness,
}

impl MoveState {
    fn initial_state(valves: &CompleteValveGraph) -> Self {
        let position = valves.index["AA"];

        Self {
            position,
            time: 0,
            curr_flow: 0,
            valve_openness: ValveOpenness::new(valves),
        }
    }

    fn is_goal(&self) -> bool {
        self.time >= 30
    }

    fn heuristic(&self, valves: &CompleteValveGraph) -> i32 {
        // Find estimate of total flow possible if we travelled to every node
        // simultaneously and turned it on
        let remaining_time = 30 - self.time;
        let mut remaining_flow = remaining_time * self.curr_flow;
        for &to in valves.non_zero_valves.iter() {
            if self.valve_openness.is_open(to) {
                continue;
            }

            let dist = valves.get_dist(self.position, to);
            if remaining_time > dist + 1 {
                let time_after_turning_on_to = remaining_time - dist - 1;
                remaining_flow += time_after_turning_on_to * valves.flow_rates[to];
            }
        }

        remaining_flow
    }

    fn expand(&self, valves: &CompleteValveGraph) -> Vec<(Self, i32)> {
        let remaining_time = 30 - self.time;
        if remaining_time <= 0 {
            return vec![];
        }

        let mut moves = Vec::with_capacity(valves.non_zero_valves.len() + 2);

        // Move consisting of just waiting until the end of 30 mins
        moves.push((
            Self {
                time: 30,
                ..self.clone()
            },
            self.curr_flow * remaining_time,
        ));

        // Move consisting of opening the valve, if the valve isn't open
        if self.valve_openness.is_closed(self.position) {
            let new_openness = self.valve_openness.set_open(self.position);
            let new_flow = valves.flow_rates[self.position];

            moves.push((
                Self {
                    time: self.time + 1,
                    position: self.position,
                    valve_openness: new_openness,
                    curr_flow: self.curr_flow + new_flow,
                },
                self.curr_flow,
            ))
        }

        if self.time == 0 || self.valve_openness.is_open(self.position) {
            // Moves consisting of changing position to any valve
            for &to in valves.non_zero_valves.iter() {
                // No reason to move to current position or valves that are
                // already open
                if to == self.position || self.valve_openness.is_open(to) {
                    continue;
                }

                let dist = valves.get_dist(self.position, to);
                if self.time + dist > 30 {
                    continue;
                }

                moves.push((
                    Self {
                        time: self.time + dist,
                        position: to,
                        ..self.clone()
                    },
                    self.curr_flow * dist,
                ))
            }
        }

        moves
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MoveState2 {
    my_position: usize,
    my_arrival: i32,
    elephant_position: usize,
    elephant_arrival: i32,
    time: i32,
    curr_flow: i32,
    valve_openness: ValveOpenness,
}

impl MoveState2 {
    fn initial_state(valves: &CompleteValveGraph) -> Self {
        let initial_position = valves.index["AA"];

        Self {
            my_position: initial_position,
            my_arrival: 0,
            elephant_position: initial_position,
            elephant_arrival: 0,
            time: 0,
            curr_flow: 0,
            valve_openness: ValveOpenness::new(valves),
        }
    }

    fn is_goal(&self) -> bool {
        self.time >= 26
    }

    fn heuristic(&self, valves: &CompleteValveGraph) -> i32 {
        let remaining_time = 26 - self.time;
        if remaining_time <= 0 || self.time > self.my_arrival || self.time > self.elephant_arrival {
            return 0;
        }

        let mut remaining_flow = remaining_time * self.curr_flow;

        // Simulate what would happen if both me and the elephant first went to
        // our destinations and turned them on
        if self.my_arrival < 26 {
            remaining_flow += (26 - self.my_arrival) * valves.flow_rates[self.my_position];
        }
        if self.elephant_arrival < 26 {
            remaining_flow +=
                (26 - self.elephant_arrival) * valves.flow_rates[self.elephant_position];
        }
        // dbg!(remaining_flow);

        // Then, find what is possible if we both took the shortest path
        // possible to every single remaining valve simultaneously
        for &to in valves.non_zero_valves.iter() {
            if to == self.my_position
                || to == self.elephant_position
                || self.valve_openness.is_open(to)
            {
                continue;
            }

            let arrival_time = (self.my_arrival + valves.get_dist(self.my_position, to) + 1)
                .min(self.elephant_arrival + valves.get_dist(self.elephant_position, to) + 1);

            if arrival_time <= 26 {
                let time_after_turning_on_to = 26 - arrival_time;
                remaining_flow += time_after_turning_on_to * valves.flow_rates[to];
            }
        }

        remaining_flow
    }

    fn expand(&self, valves: &CompleteValveGraph) -> Vec<(Self, i32)> {
        let remaining_time = 26 - self.time;
        if remaining_time <= 0 {
            return vec![];
        }

        let mut moves = Vec::with_capacity(valves.non_zero_valves.len() + 2);

        // Generate a move where me and the elephant just give up and wait
        moves.push((
            Self {
                time: 26,
                ..self.clone()
            },
            self.curr_flow * remaining_time,
        ));

        if self.my_arrival <= self.time {
            // Generate moves where I pick a target destination, move there, and
            // turn on the valve
            for &to in valves.non_zero_valves.iter() {
                // No reason to move to move valves that are already open
                if to == self.elephant_position || self.valve_openness.is_open(to) {
                    continue;
                }

                let dist = valves.get_dist(self.my_position, to);
                if self.time + dist + 1 > 26 {
                    continue;
                }

                moves.push((
                    Self {
                        my_position: to,
                        my_arrival: self.time + dist + 1,
                        ..self.clone()
                    },
                    0,
                ));
            }

            // Generate a waiting move
            moves.push((
                Self {
                    my_position: 0,
                    my_arrival: 26,
                    ..self.clone()
                },
                0,
            ));
        } else if self.elephant_arrival <= self.time {
            // Generate moves where the elephant picks a target destination,
            // moves there, and turns on the valve
            for &to in valves.non_zero_valves.iter() {
                // No reason to move to valves that are already open
                if to == self.my_position && self.valve_openness.is_open(to) {
                    continue;
                }

                let dist = valves.get_dist(self.elephant_position, to);
                if self.time + dist + 1 > 26 {
                    continue;
                }

                moves.push((
                    Self {
                        elephant_position: to,
                        elephant_arrival: self.time + dist + 1,
                        ..self.clone()
                    },
                    0,
                ));
            }

            // Generate a waiting move
            moves.push((
                Self {
                    elephant_position: 0,
                    elephant_arrival: 26,
                    ..self.clone()
                },
                0,
            ));
        } else {
            // self.time > my_arrival and self.time > elephant_arrival, we can
            // safely pass time
            let next_event = self.my_arrival.min(self.elephant_arrival);
            let elapsed_time = next_event - self.time;
            let pressure_released = elapsed_time * self.curr_flow;

            let mut new_flow = self.curr_flow;
            let mut new_openness = self.valve_openness.clone();
            if self.my_arrival == next_event && new_openness.is_closed(self.my_position) {
                // I've arrived and turned on the valve
                new_flow += valves.flow_rates[self.my_position];
                new_openness = new_openness.set_open(self.my_position);
            }
            if self.elephant_arrival == next_event && new_openness.is_closed(self.elephant_position)
            {
                // The elephant's arrived and turned on the valve
                new_flow += valves.flow_rates[self.elephant_position];
                new_openness = new_openness.set_open(self.elephant_position);
            }

            moves.push((
                Self {
                    time: next_event,
                    curr_flow: new_flow,
                    valve_openness: new_openness,
                    ..self.clone()
                },
                pressure_released,
            ));
        }

        moves
    }
}

fn a_star_max<S>(
    start_state: S,
    is_goal: impl Fn(&S) -> bool,
    expand: impl Fn(&S) -> Vec<(S, i32)>,
    heuristic: impl Fn(&S) -> i32,
) -> Option<i32>
where
    S: Clone + Eq + Hash + Debug,
{
    let mut queue = PriorityQueue::new();
    let mut max_flow_for_node = HashMap::new();

    queue.push_increase(start_state.clone(), heuristic(&start_state));
    max_flow_for_node.insert(start_state.clone(), 0);

    while let Some((state, _)) = queue.pop() {
        let curr_flow = max_flow_for_node[&state];
        if is_goal(&state) {
            return Some(curr_flow);
        }

        for (next_state, additional_flow) in expand(&state) {
            let tentative_flow = curr_flow + additional_flow;
            if tentative_flow
                > max_flow_for_node
                    .get(&next_state)
                    .copied()
                    .unwrap_or(i32::MIN)
            {
                max_flow_for_node.insert(next_state.clone(), tentative_flow);
                let f_score = tentative_flow + heuristic(&next_state);
                queue.push_increase(next_state, f_score);
            }
        }
    }

    None
}

fn star1(valves: &CompleteValveGraph) -> i32 {
    a_star_max(
        MoveState::initial_state(valves),
        |state| state.is_goal(),
        |state| state.expand(valves),
        |state| state.heuristic(valves),
    )
    .unwrap()
}

fn star2(valves: &CompleteValveGraph) -> i32 {
    a_star_max(
        MoveState2::initial_state(valves),
        |state| state.is_goal(),
        |state| state.expand(valves),
        |state| state.heuristic(valves),
    )
    .unwrap()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
