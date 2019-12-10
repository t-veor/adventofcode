#!/usr/bin/env run-cargo-script
use std::collections::HashMap;

type DirectedTree = HashMap<String, String>;

fn path_to_root<'a>(tree: &'a DirectedTree, mut node: &'a str) -> Vec<&'a str> {
    let mut result = Vec::new();
    while tree.contains_key(node) {
        node = &tree[node];
        result.push(node);
    }
    result
}

fn shortest_path_between(tree: &DirectedTree, start: &str, end: &str) -> i32 {
    let mut path1 = path_to_root(tree, start);
    let mut path2 = path_to_root(tree, end);
    path1.reverse();
    path2.reverse();

    let mut common_subpath_length = 0;
    for (i, j) in path1.iter().zip(path2.iter()) {
        if i != j {
            break
        }
        common_subpath_length += 1;
    }

    (path1.len() + path2.len() - 2 * common_subpath_length) as i32
}

fn star1(orbits: &DirectedTree) -> i32 {
    let mut count = 0;
    for mut i in orbits.keys() {
        while orbits.contains_key(i) {
            i = &orbits[i];
            count += 1;
        }
    }

    count
}

fn star2(orbits: &DirectedTree) -> i32 {
    shortest_path_between(orbits, "YOU", "SAN")
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input: DirectedTree = std::fs::read_to_string(filename)
        .unwrap()
        .trim()
        .split_whitespace()
        .map(|l| {
            let mut orbit = l.split(")");
            let parent = orbit.next().unwrap();
            let child = orbit.next().unwrap();
            (child.to_owned(), parent.to_owned())
        })
        .collect();

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
