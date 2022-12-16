#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! regex = "1"
//! priority-queue = "1.3.0"
//! ```

use priority_queue::PriorityQueue;
use regex::Regex;
use std::collections::HashMap;

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

        Self {
            index: valves.index.clone(),
            flow_rates: valves.flow_rates.clone(),
            dist,
        }
    }

    fn get_dist(&self, from: usize, to: usize) -> i32 {
        let node_count = self.flow_rates.len();
        let idx = from * node_count + to;
        self.dist[idx]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MoveState {
    position: usize,
    time: i32,
    total_flow: i32,
    curr_flow: i32,
    valve_openness: Vec<bool>,
}

impl MoveState {
    fn initial_state(valves: &CompleteValveGraph) -> Self {
        let position = valves.index["AA"];
        let total_flow = valves.flow_rates.iter().sum();

        Self {
            position,
            time: 0,
            total_flow,
            curr_flow: 0,
            valve_openness: vec![false; valves.flow_rates.len()],
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
        for to in 0..valves.flow_rates.len() {
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

        let mut moves = Vec::with_capacity(valves.flow_rates.len() + 1);

        // Move consisting of just waiting until the end of 30 mins
        moves.push((
            Self {
                time: 30,
                ..self.clone()
            },
            self.curr_flow * remaining_time,
        ));

        // Move consisting of opening the valve, if the valve isn't open
        if !self.valve_openness[self.position] {
            let mut new_openness = self.valve_openness.clone();
            new_openness[self.position] = true;
            let new_flow = valves.flow_rates[self.position];

            moves.push((
                Self {
                    time: self.time + 1,
                    position: self.position,
                    valve_openness: new_openness,
                    total_flow: self.total_flow,
                    curr_flow: self.curr_flow + new_flow,
                },
                self.curr_flow,
            ))
        }

        if self.time == 0 || self.valve_openness[self.position] {
            // Moves consisting of changing position to any valve
            for to in 0..valves.flow_rates.len() {
                if to == self.position {
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

fn a_star(valves: &CompleteValveGraph) -> i32 {
    // let mut explored_nodes = 0;

    let mut queue = PriorityQueue::new();
    let mut max_flow_for_node = HashMap::new();

    // let mut came_from = HashMap::<MoveState, MoveState>::new();

    let start_state = MoveState::initial_state(valves);
    queue.push_increase(start_state.clone(), start_state.heuristic(valves));
    max_flow_for_node.insert(start_state.clone(), 0);

    while let Some((state, _)) = queue.pop() {
        // explored_nodes += 1;
        // if explored_nodes % 1000 == 0 {
        //     println!("explored {explored_nodes}");
        // }

        let curr_flow = max_flow_for_node[&state];
        if state.is_goal() {
            // let mut path = vec![(state.position, curr_flow)];
            // let mut current = state;
            // while let Some(prev) = came_from.get(&current) {
            //     current = prev.clone();
            //     path.push((current.position, max_flow_for_node[&current]));
            // }
            // path.reverse();
            // println!("{:?}", path);

            return curr_flow;
        }

        for (next_state, additional_flow) in state.expand(valves) {
            let tentative_flow = curr_flow + additional_flow;
            if tentative_flow
                > max_flow_for_node
                    .get(&next_state)
                    .copied()
                    .unwrap_or(i32::MIN)
            {
                // came_from.insert(next_state.clone(), state.clone());
                max_flow_for_node.insert(next_state.clone(), tentative_flow);
                let f_score = tentative_flow + next_state.heuristic(valves);
                queue.push_increase(next_state, f_score);
            }
        }
    }

    panic!()
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

fn star1(input: &CompleteValveGraph) -> i32 {
    a_star(input)
}

fn star2(input: &CompleteValveGraph) -> i32 {
    todo!()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
