use std::collections::HashMap;

use rand::{thread_rng, Rng};
use utils::read_input_file;

#[derive(Debug, Clone)]
struct Graph {
    vertices: Vec<(String, usize)>,
    adjacency: HashMap<String, Vec<String>>,
}

impl Graph {
    fn random_edge(&self) -> (&str, &str) {
        let edge_choices: Vec<_> = self.adjacency.iter().collect();
        // Overcount by 2x but it doesn't matter for sampling
        let edge_count = edge_choices.iter().map(|(_, es)| es.len()).sum::<usize>();
        let mut edge_idx = thread_rng().gen_range(0..edge_count);

        let mut chosen_edge = None;
        for (u, es) in edge_choices {
            if edge_idx < es.len() {
                let v = es[edge_idx].as_str();
                chosen_edge = Some((u.as_str(), v));
                break;
            } else {
                edge_idx -= es.len();
            }
        }

        chosen_edge.unwrap()
    }

    fn contract(&mut self, u: &str, v: &str) {
        assert!(u != v);

        let u_idx = self.vertices.iter().position(|(i, _)| i == u).unwrap();
        let v_idx = self.vertices.iter().position(|(i, _)| i == v).unwrap();

        let v_count = self.vertices[v_idx].1;
        self.vertices[u_idx].1 += v_count;
        self.vertices.swap_remove(v_idx);

        let replace_edges = self.adjacency[v].clone();
        let mut vertices_to_scrub_v_from = vec![u.to_string()];

        for w in replace_edges {
            if w != u {
                if !vertices_to_scrub_v_from.contains(&w) {
                    vertices_to_scrub_v_from.push(w.clone());
                }

                self.adjacency.get_mut(&w).unwrap().push(u.to_string());
                self.adjacency.get_mut(u).unwrap().push(w);
            }
        }

        for w in vertices_to_scrub_v_from {
            self.adjacency.get_mut(&w).unwrap().retain(|i| i != v);
        }

        self.adjacency.remove(v);
    }
}

fn parse_input(input: &str) -> Graph {
    let mut adjacency: HashMap<String, Vec<String>> = HashMap::new();

    for line in input.lines() {
        let (parent, children) = line.split_once(": ").unwrap();
        for child in children.split(' ') {
            adjacency
                .entry(parent.to_string())
                .or_default()
                .push(child.to_string());
            adjacency
                .entry(child.to_string())
                .or_default()
                .push(parent.to_string());
        }
    }

    let vertices = adjacency.keys().cloned().map(|i| (i, 1)).collect();

    Graph {
        vertices,
        adjacency,
    }
}

#[derive(Debug)]
struct CutData {
    cut_size: usize,
    sizes: (usize, usize),
}

fn contract_until(graph: &mut Graph, required_vertices: usize) {
    while graph.vertices.len() > required_vertices {
        let (u, v) = graph.random_edge();
        let (u, v) = (u.to_string(), v.to_string());
        graph.contract(&u, &v);
    }
}

fn kargers_algorithm_iteration(mut graph: Graph) -> CutData {
    contract_until(&mut graph, 2);

    let supernode_a = graph.vertices[0].1;
    let supernode_b = graph.vertices[1].1;

    let cut_size = graph.adjacency[&graph.vertices[0].0].len();

    CutData {
        cut_size,
        sizes: (supernode_a, supernode_b),
    }
}

fn kargers_algorithm(graph: &Graph, required_cut_size: usize) -> CutData {
    loop {
        let cut_data = kargers_algorithm_iteration(graph.clone());
        if cut_data.cut_size <= required_cut_size {
            return cut_data;
        }
    }
}

fn star1(graph: &Graph) -> usize {
    let cut_data = kargers_algorithm(graph, 3);
    cut_data.sizes.0 * cut_data.sizes.1
}

fn main() {
    let input = read_input_file!();
    let graph = parse_input(&input);

    println!("{}", star1(&graph));
}
