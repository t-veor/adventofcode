use std::collections::{HashMap, HashSet};

use utils::read_input_file;

// Terribly inefficient but it works

#[derive(Clone)]
struct Graph {
    adjacency: HashMap<String, Vec<String>>,
}

fn brute_force_3_cliques(graph: &Graph) -> Vec<[&String; 3]> {
    let nodes: Vec<_> = graph.adjacency.keys().collect();
    let mut cliques = HashSet::new();

    for a in nodes {
        let neighbors = graph.adjacency[a].as_slice();
        for i in 0..neighbors.len() {
            for j in i + 1..neighbors.len() {
                let (b, c) = (&neighbors[i], &neighbors[j]);

                if graph.adjacency[b].contains(c) {
                    let mut clique = [a, b, c];
                    clique.sort();
                    cliques.insert(clique);
                }
            }
        }
    }

    cliques.into_iter().collect()
}

fn bron_kerbosch_recurse<'a>(
    graph: &'a Graph,
    required_vertices: HashSet<&'a String>,
    mut potential_vertices: HashSet<&'a String>,
    mut exclude_vertices: HashSet<&'a String>,
    output: &mut Vec<Vec<&'a String>>,
) {
    if potential_vertices.is_empty() && exclude_vertices.is_empty() {
        return output.push(required_vertices.into_iter().collect());
    }

    while let Some(&v) = potential_vertices.iter().next() {
        let neighbors: HashSet<&String> = graph.adjacency[v].iter().collect();

        bron_kerbosch_recurse(
            graph,
            {
                let mut next_required_vertices = required_vertices.clone();
                next_required_vertices.insert(v);
                next_required_vertices
            },
            potential_vertices
                .intersection(&neighbors)
                .copied()
                .collect(),
            exclude_vertices.intersection(&neighbors).copied().collect(),
            output,
        );

        potential_vertices.remove(v);
        exclude_vertices.insert(v);
    }
}

fn bron_kerbosch(graph: &Graph) -> Vec<Vec<&String>> {
    let mut output = Vec::new();

    bron_kerbosch_recurse(
        graph,
        HashSet::new(),
        graph.adjacency.keys().collect(),
        HashSet::new(),
        &mut output,
    );

    output
}

fn parse_input(input: &str) -> Graph {
    let mut adjacency: HashMap<String, Vec<String>> = HashMap::new();

    for line in input.lines() {
        let (a, b) = line.split_once('-').unwrap();
        let (a, b) = (a.to_string(), b.to_string());

        adjacency.entry(a.clone()).or_default().push(b.clone());
        adjacency.entry(b).or_default().push(a);
    }

    Graph { adjacency }
}

fn star1(graph: &Graph) -> usize {
    brute_force_3_cliques(graph)
        .into_iter()
        .filter(|clique| clique.iter().any(|i| i.starts_with('t')))
        .count()
}

fn star2(graph: &Graph) -> String {
    let cliques = bron_kerbosch(graph);
    let mut max_clique: Vec<&str> = cliques
        .iter()
        .max_by_key(|i| i.len())
        .unwrap()
        .iter()
        .map(|i| i.as_str())
        .collect();
    max_clique.sort();
    max_clique.join(",")
}

fn main() {
    let input = read_input_file!();
    let graph = parse_input(&input);

    println!("{}", star1(&graph));
    println!("{}", star2(&graph));
}
