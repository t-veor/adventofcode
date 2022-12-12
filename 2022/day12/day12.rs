#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashSet},
};

struct HeightMap {
    map: Vec<u8>,
    width: usize,
    height: usize,
}

impl HeightMap {
    fn idx(&self, pos: (usize, usize)) -> usize {
        pos.0 + pos.1 * self.width
    }

    fn get(&self, pos: (usize, usize)) -> u8 {
        self.map[self.idx(pos)]
    }

    fn adjacent(&self, pos: (usize, usize)) -> Vec<(usize, usize)> {
        let mut adjacent_nodes = Vec::with_capacity(4);

        let (x, y) = pos;
        if x > 0 {
            adjacent_nodes.push((x - 1, y))
        }
        if x + 1 < self.width {
            adjacent_nodes.push((x + 1, y))
        }
        if y > 0 {
            adjacent_nodes.push((x, y - 1))
        }
        if y + 1 < self.height {
            adjacent_nodes.push((x, y + 1))
        }

        adjacent_nodes
    }
}

fn parse_input(input: String) -> (HeightMap, (usize, usize), (usize, usize)) {
    let mut start = (0, 0);
    let mut end = (0, 0);
    let mut heightmap = Vec::new();
    let mut width = 0;
    let mut height = 0;

    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.bytes().enumerate() {
            let elevation = match c {
                b'S' => {
                    start = (x, y);
                    0
                }
                b'E' => {
                    end = (x, y);
                    25
                }
                b'a'..=b'z' => c - b'a',
                _ => unreachable!(),
            };

            heightmap.push(elevation);
        }

        if line.len() > 0 {
            width = line.len();
            height += 1;
        }
    }

    (
        HeightMap {
            map: heightmap,
            width,
            height,
        },
        start,
        end,
    )
}

fn dijkstra<F, G>(map: &HeightMap, start: (usize, usize), adjacent: F, is_goal: G) -> Option<usize>
where
    F: Fn(&HeightMap, (usize, usize)) -> Vec<(usize, usize)>,
    G: Fn(&HeightMap, (usize, usize)) -> bool,
{
    let mut priority_queue = BinaryHeap::new();
    let mut dists = vec![usize::MAX; map.map.len()];
    let mut seen = HashSet::new();

    priority_queue.push((Reverse(0), start));
    dists[map.idx(start)] = 0;

    while let Some((Reverse(dist), node)) = priority_queue.pop() {
        if is_goal(map, node) {
            return Some(dist);
        }

        if !seen.insert(node) {
            continue;
        }

        let neighbors = adjacent(map, node);
        for neighbor in neighbors {
            if dist + 1 < dists[map.idx(neighbor)] {
                dists[map.idx(neighbor)] = dist + 1;
                priority_queue.push((Reverse(dist + 1), neighbor))
            }
        }
    }

    None
}

fn star1(input: &(HeightMap, (usize, usize), (usize, usize))) -> usize {
    let (map, start, end) = input;
    dijkstra(
        map,
        *start,
        |map, pos| {
            let elevation = map.get(pos);
            let mut neighbors = map.adjacent(pos);
            neighbors.retain(|neighbor| map.get(*neighbor) <= elevation + 1);
            neighbors
        },
        |map, pos| pos == *end,
    )
    .unwrap()
}

fn star2(input: &(HeightMap, (usize, usize), (usize, usize))) -> usize {
    let (map, _start, end) = input;
    dijkstra(
        map,
        *end,
        |map, pos| {
            let elevation = map.get(pos);
            let mut neighbors = map.adjacent(pos);
            neighbors.retain(|neighbor| map.get(*neighbor) + 1 >= elevation);
            neighbors
        },
        |map, pos| map.get(pos) == 0,
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
