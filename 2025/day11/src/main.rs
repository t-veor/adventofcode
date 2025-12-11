use std::collections::{HashMap, HashSet};

use utils::read_input_file;

fn parse_input(input: &str) -> HashMap<String, Vec<String>> {
    let mut graph = HashMap::new();

    for line in input.lines() {
        let (head, body) = line.split_once(": ").unwrap();

        let body = body.split_whitespace().map(String::from).collect();

        graph.insert(String::from(head), body);
    }

    graph
}

fn topo_sort_from(node: &str, graph: &HashMap<String, Vec<String>>) -> Vec<String> {
    fn recurse(
        node: &str,
        visited: &mut HashSet<String>,
        acc: &mut Vec<String>,
        graph: &HashMap<String, Vec<String>>,
    ) {
        if !visited.insert(node.to_owned()) {
            return;
        }

        let children = graph
            .get(node)
            .map(|children| children.as_slice())
            .unwrap_or(&[]);
        for child in children {
            recurse(child, visited, acc, graph);
        }

        acc.push(node.to_owned());
    }

    let mut sorted = Vec::new();

    recurse(node, &mut HashSet::new(), &mut sorted, graph);

    sorted.reverse();
    sorted
}

fn paths_between(
    from: &str,
    to: &str,
    topo_ordering: &[String],
    graph: &HashMap<String, Vec<String>>,
) -> u64 {
    let from_idx = topo_ordering.iter().position(|s| s == from).unwrap();
    let to_idx = topo_ordering.iter().position(|s| s == to).unwrap();

    assert!(from_idx <= to_idx);

    let mut paths = HashMap::new();
    paths.insert(from.to_owned(), 1u64);

    for node in &topo_ordering[from_idx..to_idx] {
        let paths_to_node = paths.get(node).copied().unwrap_or(0);
        let children = graph
            .get(node)
            .map(|children| children.as_slice())
            .unwrap_or(&[]);

        for child in children {
            *paths.entry(child.clone()).or_default() += paths_to_node;
        }
    }

    paths.get(to).copied().unwrap_or(0)
}

fn star1(graph: &HashMap<String, Vec<String>>) -> u64 {
    let topo_ordering = topo_sort_from("you", graph);
    paths_between("you", "out", &topo_ordering, graph)
}

fn star2(graph: &HashMap<String, Vec<String>>) -> u64 {
    // We have to assume that there is a path either:
    // svr -> fft -> dac -> out
    // or
    // svt -> dac -> fft -> out
    // or there would be no paths.

    // In addition, either fft -> dac or dac -> fft but not both, because then there would be
    // a cycle.
    let topo_ordering = topo_sort_from("svr", graph);

    let fft_pos = topo_ordering.iter().position(|s| s == "fft").unwrap();
    let dac_pos = topo_ordering.iter().position(|s| s == "dac").unwrap();

    // Find the earlier of fft_pos and dac_pos
    let (first_checkpoint, second_checkpoint) = if fft_pos < dac_pos {
        ("fft", "dac")
    } else {
        ("dac", "fft")
    };

    paths_between("svr", first_checkpoint, &topo_ordering, graph)
        * paths_between(first_checkpoint, second_checkpoint, &topo_ordering, graph)
        * paths_between(second_checkpoint, "out", &topo_ordering, graph)
}

fn main() {
    let input = read_input_file!();
    let graph = parse_input(&input);

    println!("{}", star1(&graph));
    println!("{}", star2(&graph));
}
