use core::panic;
use std::collections::HashMap;

use utils::read_input_file;

fn parse_input(input: &str) -> (HashMap<i32, Vec<i32>>, Vec<Vec<i32>>) {
    let (ordering_rules, orderings) = input.trim().split_once("\n\n").unwrap();

    let mut ordering_rules_hash: HashMap<i32, Vec<i32>> = HashMap::new();

    for line in ordering_rules.lines() {
        let (a, b) = line.split_once('|').unwrap();
        let (a, b): (i32, i32) = (a.parse().unwrap(), b.parse().unwrap());

        ordering_rules_hash.entry(a).or_default().push(b);
    }

    let orderings = orderings
        .lines()
        .map(|line| line.split(',').map(|i| i.parse().unwrap()).collect())
        .collect();

    (ordering_rules_hash, orderings)
}

fn check_ordering(ordering_rules: &HashMap<i32, Vec<i32>>, ordering: &[i32]) -> bool {
    for i in 0..ordering.len() {
        for j in i + 1..ordering.len() {
            let (a, b) = (ordering[i], ordering[j]);
            // Both (a, b) showing up in the ordering rules or not is fine, so
            // we only need to check the reverse order (b, a) does not exist in
            // the ordering rules
            match ordering_rules.get(&b) {
                Some(children) => {
                    if children.contains(&a) {
                        return false;
                    }
                }
                None => (),
            }
        }
    }

    true
}

fn extract_subgraph(
    ordering_rules: &HashMap<i32, Vec<i32>>,
    ordering: &[i32],
) -> HashMap<i32, Vec<i32>> {
    let mut subgraph = HashMap::new();

    for &i in ordering {
        if let Some(children) = ordering_rules.get(&i) {
            subgraph.insert(
                i,
                children
                    .iter()
                    .copied()
                    .filter(|i| ordering.contains(i))
                    .collect(),
            );
        }
    }

    subgraph
}

fn topo_sort(subgraph: &HashMap<i32, Vec<i32>>, ordering: &[i32]) -> Vec<i32> {
    let mut sorted = Vec::with_capacity(ordering.len());

    fn visit(
        node: i32,
        subgraph: &HashMap<i32, Vec<i32>>,
        sorted: &mut Vec<i32>,
        visited: &mut Vec<i32>,
    ) {
        if sorted.contains(&node) {
            return;
        }
        if visited.contains(&node) {
            panic!("Cycle in input!");
        }

        visited.push(node);

        let children = subgraph
            .get(&node)
            .map(|i| i.as_slice())
            .unwrap_or_default();
        for &child in children {
            visit(child, subgraph, sorted, visited);
        }

        sorted.push(node);
    }

    'outer: loop {
        for i in ordering {
            if !sorted.contains(i) {
                visit(*i, subgraph, &mut sorted, &mut Vec::new());
                continue 'outer;
            }
        }
        break;
    }

    sorted.reverse();
    sorted
}

fn star1(ordering_rules: &HashMap<i32, Vec<i32>>, orderings: &[Vec<i32>]) -> i32 {
    let mut total = 0;

    for ordering in orderings {
        if check_ordering(ordering_rules, ordering) {
            total += ordering[ordering.len() / 2];
        }
    }

    total
}

fn star2(ordering_rules: &HashMap<i32, Vec<i32>>, orderings: &[Vec<i32>]) -> i32 {
    let mut incorrect_orderings = Vec::new();

    for ordering in orderings {
        if !check_ordering(ordering_rules, ordering) {
            incorrect_orderings.push(ordering);
        }
    }

    let mut total = 0;
    for ordering in incorrect_orderings {
        let subgraph = extract_subgraph(ordering_rules, ordering);
        let correct_ordering = topo_sort(&subgraph, ordering);

        total += correct_ordering[correct_ordering.len() / 2];
    }
    total
}

fn main() {
    let input = read_input_file!();
    let (ordering_rules, orderings) = parse_input(&input);

    println!("{}", star1(&ordering_rules, &orderings));
    println!("{}", star2(&ordering_rules, &orderings));
}
