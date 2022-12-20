#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

#[derive(Debug, Clone)]
struct CircularLinkedList {
    nodes: Vec<CLLNode>,
}

#[derive(Debug, Clone)]
struct CLLNode {
    value: i64,
    left: usize,
    right: usize,
}

impl CircularLinkedList {
    fn new(values: &[i64]) -> Self {
        let mut nodes = Vec::with_capacity(values.len());
        for i in 0..values.len() {
            let left = if i > 0 { i - 1 } else { values.len() - 1 };
            let right = if i < values.len() - 1 { i + 1 } else { 0 };
            nodes.push(CLLNode {
                value: values[i],
                left,
                right,
            });
        }

        Self { nodes }
    }

    fn collect_from_original_idx(&self, idx: usize) -> Option<Vec<i64>> {
        if idx >= self.nodes.len() {
            return None;
        }

        let mut result = Vec::with_capacity(self.nodes.len());
        let mut curr = idx;
        loop {
            result.push(self.nodes[curr].value);
            curr = self.nodes[curr].right;

            if curr == idx {
                break;
            }
        }

        Some(result)
    }

    fn collect_from_elem(&self, elem: i64) -> Option<Vec<i64>> {
        self.nodes
            .iter()
            .position(|node| node.value == elem)
            .and_then(|idx| self.collect_from_original_idx(idx))
    }

    fn mix_once(&mut self, original_idx: usize) {
        let CLLNode { value, left, right } = self.nodes[original_idx];
        let mix_amount = value % (self.nodes.len() - 1) as i64;
        if mix_amount > 0 {
            // remove
            self.nodes[left].right = right;
            self.nodes[right].left = left;
            // walk
            let mut curr = right;
            for _ in 0..mix_amount {
                curr = self.nodes[curr].right;
            }
            // insert
            let new_left = self.nodes[curr].left;
            let new_right = curr;
            self.nodes[new_left].right = original_idx;
            self.nodes[new_right].left = original_idx;
            self.nodes[original_idx].left = new_left;
            self.nodes[original_idx].right = new_right;
        } else if mix_amount < 0 {
            // remove
            self.nodes[left].right = right;
            self.nodes[right].left = left;
            // walk
            let mut curr = left;
            for _ in 0..-mix_amount {
                curr = self.nodes[curr].left;
            }
            // insert
            let new_left = curr;
            let new_right = self.nodes[curr].right;
            self.nodes[new_left].right = original_idx;
            self.nodes[new_right].left = original_idx;
            self.nodes[original_idx].left = new_left;
            self.nodes[original_idx].right = new_right;
        }
    }

    fn mix(&mut self) {
        for i in 0..self.nodes.len() {
            self.mix_once(i);
        }
    }
}

fn parse_input(input: String) -> Vec<i64> {
    input.lines().map(|i| i.parse().unwrap()).collect()
}

fn star1(input: &[i64]) -> i64 {
    let mut ll = CircularLinkedList::new(input);
    ll.mix();

    let flattened = ll.collect_from_elem(0).unwrap();
    flattened[1000 % flattened.len()]
        + flattened[2000 % flattened.len()]
        + flattened[3000 % flattened.len()]
}

fn star2(input: &[i64]) -> i64 {
    const DECRYPTION_KEY: i64 = 811589153;
    let mut ll = CircularLinkedList::new(
        &input
            .iter()
            .map(|&x| x * DECRYPTION_KEY)
            .collect::<Vec<_>>(),
    );

    for _ in 0..10 {
        ll.mix();
    }

    let flattened = ll.collect_from_elem(0).unwrap();
    flattened[1000 % flattened.len()]
        + flattened[2000 % flattened.len()]
        + flattened[3000 % flattened.len()]
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
