#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct State {
    elevator_pos: u8,
    // Convention: every even index is the position of a microchip, and the next
    // index is the position of its generator
    item_positions: Vec<u8>,
}

impl State {
    // Any pair of microchip-generator is interchangeable with any other, so if
    // we put them into a standard order it prunes the number of states by _a
    // lot_ (6m -> 15k for part 2)
    fn normalized(mut self) -> Self {
        let mut grouped: Vec<[u8; 2]> = self
            .item_positions
            .chunks_exact(2)
            .map(|chunk| chunk.try_into().unwrap())
            .collect();
        grouped.sort_unstable();
        self.item_positions = grouped.into_iter().flat_map(|group| group).collect();
        self
    }

    fn is_valid(&self) -> bool {
        // Check all microchips are safe
        for i in (0..self.item_positions.len()).step_by(2) {
            if self.item_positions[i] == self.item_positions[i + 1] {
                // generator is powering this chip, fine
                continue;
            }
            for j in (0..self.item_positions.len()).skip(1).step_by(2) {
                if self.item_positions[i] == self.item_positions[j] {
                    // chip is on same floor as a generator (and not being
                    // powered by own generator)
                    return false;
                }
            }
        }

        true
    }

    #[allow(unused)]
    fn debug_print(&self) {
        for floor in (0..4).rev() {
            print!("F{} ", floor + 1);
            if self.elevator_pos == floor {
                print!("E  ");
            } else {
                print!(".  ");
            }
            for (idx, item_pos) in self.item_positions.iter().enumerate() {
                if floor == *item_pos {
                    let item_name = char::from_u32('A' as u32 + idx as u32 / 2).unwrap();
                    let item_type = if idx % 2 == 0 { 'M' } else { 'G' };
                    print!("{}{} ", item_name, item_type);
                } else {
                    print!(".  ");
                }
            }
            println!()
        }
    }

    fn double_move(&self, to: u8, idx1: usize, idx2: usize) -> Option<Self> {
        let mut next_state = Self {
            elevator_pos: to,
            item_positions: self.item_positions.clone(),
        };
        next_state.item_positions[idx1] = to;
        next_state.item_positions[idx2] = to;
        next_state.is_valid().then(|| next_state.normalized())
    }

    fn is_goal(&self) -> bool {
        self.item_positions.iter().all(|&i| i == 3)
    }

    fn expand(&self) -> Vec<Self> {
        let mut result = Vec::new();

        let objects_on_curr_floor: Vec<_> = self
            .item_positions
            .iter()
            .enumerate()
            .filter_map(|(idx, floor)| (*floor == self.elevator_pos).then_some(idx))
            .collect();

        // Actually, we only need to generate double moves, because a double
        // move when both i and j are the same is the same as a single move
        if self.elevator_pos > 0 {
            let to = self.elevator_pos - 1;
            // double moves down
            result.extend(objects_on_curr_floor.iter().flat_map(|&i| {
                objects_on_curr_floor
                    .iter()
                    .filter_map(move |&j| self.double_move(to, i, j))
            }));
        }
        if self.elevator_pos < 3 {
            let to = self.elevator_pos + 1;
            // double moves up
            result.extend(objects_on_curr_floor.iter().flat_map(|&i| {
                objects_on_curr_floor
                    .iter()
                    .filter_map(move |&j| self.double_move(to, i, j))
            }));
        }

        result
    }
}

fn parse_input(input: &str) -> State {
    #[derive(Debug)]
    struct Item<'a> {
        element: &'a str,
        is_generator: bool,
    }

    fn get_items(line: &str) -> Vec<Item<'_>> {
        let split: Vec<_> = line.split_ascii_whitespace().collect();

        let mut result = Vec::new();
        let mut i = 4;
        while i < split.len() {
            if split[i] == "a" || split[i] == "and" {
                i += 1;
                continue;
            }

            if split[i] == "nothing" {
                break;
            }

            if let Some(element) = split[i].strip_suffix("-compatible") {
                result.push(Item {
                    element,
                    is_generator: false,
                });
            } else {
                result.push(Item {
                    element: split[i],
                    is_generator: true,
                });
            }
            i += 2;
        }

        result
    }

    let items_by_floor: Vec<_> = input.lines().map(get_items).collect();

    let mut elements = HashMap::new();
    for item in items_by_floor.iter().flat_map(|i| i) {
        if !elements.contains_key(item.element) {
            elements.insert(item.element, elements.len());
        }
    }

    let mut state = State {
        elevator_pos: 0,
        item_positions: vec![0; elements.len() * 2],
    };
    for (floor, items_by_floor) in items_by_floor.into_iter().enumerate() {
        for item in items_by_floor {
            let mut idx = elements[item.element] * 2;
            if item.is_generator {
                idx += 1
            };
            state.item_positions[idx] = floor as u8;
        }
    }
    state
}

fn bfs<T>(
    initial_state: T,
    is_goal: impl Fn(&T) -> bool,
    expand: impl Fn(&T) -> Vec<T>,
) -> Option<usize>
where
    T: Eq + Hash + Clone,
{
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();

    visited.insert(initial_state.clone());
    queue.push_back((initial_state, 0));

    while let Some((state, depth)) = queue.pop_front() {
        if is_goal(&state) {
            return Some(depth);
        }

        for next_state in expand(&state) {
            if visited.insert(next_state.clone()) {
                queue.push_back((next_state, depth + 1));
            }
        }
    }

    None
}

fn star1(input: &State) -> usize {
    bfs(
        input.clone().normalized(),
        |state| state.is_goal(),
        |state| state.expand(),
    )
    .unwrap()
}

// Well, part 2 multiplies the state space by 64 :|
// It's still fast enough that I can just wait for the answer, but it's not
// ideal.

fn star2(input: &State) -> usize {
    let mut modified_state = input.clone();
    modified_state
        .item_positions
        .extend_from_slice(&[0, 0, 0, 0]);

    bfs(
        modified_state.normalized(),
        |state| state.is_goal(),
        |state| state.expand(),
    )
    .unwrap()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
