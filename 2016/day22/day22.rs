#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
};

#[derive(Debug, Clone, Copy)]
struct Node {
    size: i32,
    used: i32,
}

fn parse_input(input: &str) -> HashMap<(i32, i32), Node> {
    let mut nodes = HashMap::new();

    for line in input.lines().skip(2) {
        let split: Vec<_> = line.split_ascii_whitespace().collect();
        let node_name = split[0].split('/').last().unwrap();
        let node_name_split: Vec<_> = node_name.split('-').collect();
        let x = node_name_split[1]
            .strip_prefix("x")
            .unwrap()
            .parse()
            .unwrap();
        let y = node_name_split[2]
            .strip_prefix("y")
            .unwrap()
            .parse()
            .unwrap();

        let size = split[1].strip_suffix("T").unwrap().parse().unwrap();
        let used = split[2].strip_suffix("T").unwrap().parse().unwrap();

        nodes.insert((x, y), Node { size, used });
    }

    nodes
}

fn star1(input: &HashMap<(i32, i32), Node>) -> i32 {
    let mut count = 0;

    // You can do this in O(n log n) by sorting the nodes by avail and doing a
    // binary search for each node, but whatever
    let keys: Vec<_> = input.keys().collect();
    for i in 0..keys.len() {
        for j in 0..keys.len() {
            if i == j {
                continue;
            }

            let from = input[keys[i]];
            let to = input[keys[j]];

            if from.used > 0 && from.used <= to.size - to.used {
                count += 1;
            }
        }
    }

    count
}

// The instructions heavily imply that there are only 3 kinds of nodes - empty
// nodes, nodes that can be moved, and unmoveable nodes. This reduces the
// problem down to a sliding block puzzle.
#[derive(Debug, Clone)]
struct SlidingBlockMap {
    width: i32,
    height: i32,
    impassable: HashSet<(i32, i32)>,
    initial_empty_pos: (i32, i32),
}

impl SlidingBlockMap {
    fn new(map: &HashMap<(i32, i32), Node>) -> Self {
        let mut movable = HashSet::new();
        let mut receivable = HashSet::new();
        let keys: Vec<_> = map.keys().collect();
        for i in 0..keys.len() {
            for j in 0..keys.len() {
                if i == j {
                    continue;
                }

                let from = map[keys[i]];
                let to = map[keys[j]];

                if from.used > 0 && from.used <= to.size - to.used {
                    movable.insert(keys[i]);
                    receivable.insert(keys[j]);
                }
            }
        }

        assert_eq!(receivable.len(), 1);

        let impassable = map
            .keys()
            .copied()
            .filter(|pos| !movable.contains(pos))
            .collect();

        let max_x = map.keys().map(|(x, _)| *x).max().unwrap();
        let max_y = map.keys().map(|(_, y)| *y).max().unwrap();

        Self {
            width: max_x + 1,
            height: max_y + 1,
            impassable,
            initial_empty_pos: *receivable.into_iter().next().unwrap(),
        }
    }

    fn passable(&self, (x, y): (i32, i32)) -> bool {
        (0..self.width).contains(&x)
            && (0..self.height).contains(&y)
            && !self.impassable.contains(&(x, y))
    }

    fn initial_state(&self) -> State {
        State {
            empty_pos: self.initial_empty_pos,
            goal_pos: (self.width - 1, 0),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
    empty_pos: (i32, i32),
    goal_pos: (i32, i32),
}

impl State {
    fn is_goal(&self) -> bool {
        self.goal_pos == (0, 0)
    }

    fn expand(&self, map: &SlidingBlockMap) -> Vec<Self> {
        let mut children = Vec::with_capacity(4);

        const DELTAS: [(i32, i32); 4] = [(1, 0), (-1, 0), (0, 1), (0, -1)];
        for (dx, dy) in DELTAS {
            let next_empty_pos = (self.empty_pos.0 + dx, self.empty_pos.1 + dy);
            if map.passable(next_empty_pos) {
                children.push(Self {
                    empty_pos: next_empty_pos,
                    goal_pos: if next_empty_pos == self.goal_pos {
                        self.empty_pos
                    } else {
                        self.goal_pos
                    },
                })
            }
        }

        children
    }

    #[allow(unused)]
    fn debug_print(&self, map: &SlidingBlockMap) {
        for y in 0..map.height {
            for x in 0..map.width {
                if (x, y) == self.empty_pos {
                    print!("_")
                } else if (x, y) == self.goal_pos {
                    print!("G")
                } else if map.impassable.contains(&(x, y)) {
                    print!("#")
                } else {
                    print!(".")
                }
            }
            println!()
        }
    }
}

fn bfs<T, I>(initial_state: T, is_goal: impl Fn(&T) -> bool, expand: impl Fn(&T) -> I) -> Option<T>
where
    T: Eq + Hash + Clone,
    I: IntoIterator<Item = T>,
{
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();

    visited.insert(initial_state.clone());
    queue.push_back(initial_state);

    while let Some(state) = queue.pop_front() {
        if is_goal(&state) {
            return Some(state);
        }

        for next_state in expand(&state) {
            if visited.insert(next_state.clone()) {
                queue.push_back(next_state);
            }
        }
    }

    None
}

fn star2(input: &HashMap<(i32, i32), Node>) -> i32 {
    let map = SlidingBlockMap::new(input);

    bfs(
        (map.initial_state(), 0),
        |(state, _)| state.is_goal(),
        |(state, depth)| {
            let depth = *depth;
            state
                .expand(&map)
                .into_iter()
                .map(move |new_state| (new_state, depth + 1))
        },
    )
    .unwrap()
    .1
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
