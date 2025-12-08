use utils::{
    disjoint_set::DisjointSet,
    glam::{I64Vec3, i64vec3},
    read_input_file,
};

fn parse_input(input: &str) -> Vec<I64Vec3> {
    input
        .lines()
        .map(|line| {
            let &[x, y, z] = line
                .split(',')
                .map(|i| i.parse().unwrap())
                .collect::<Vec<_>>()
                .as_slice()
            else {
                panic!("Invalid vector: {line}")
            };

            i64vec3(x, y, z)
        })
        .collect()
}

fn sorted_edges(nodes: &[I64Vec3]) -> Vec<(usize, usize)> {
    if nodes.is_empty() {
        return Vec::new();
    }

    let mut edges = Vec::with_capacity(nodes.len() * (nodes.len() - 1) / 2);
    for i in 0..nodes.len() {
        for j in i + 1..nodes.len() {
            edges.push((i, j));
        }
    }

    edges.sort_by_key(|&(i, j)| nodes[i].distance_squared(nodes[j]));

    edges
}

fn star1(nodes: &[I64Vec3]) -> i64 {
    let edges = sorted_edges(nodes);

    let mut disjoint_set: DisjointSet<_> = (0..nodes.len()).collect();

    for (u, v) in edges.iter().take(1000) {
        disjoint_set.union(u, v);
    }

    let mut component_sizes: Vec<_> = disjoint_set.sets_with_sizes().values().copied().collect();
    component_sizes.sort();
    component_sizes.reverse();

    component_sizes.iter().take(3).map(|&i| i as i64).product()
}

fn star2(nodes: &[I64Vec3]) -> i64 {
    let edges = sorted_edges(nodes);

    let mut disjoint_set: DisjointSet<_> = (0..nodes.len()).collect();

    for (u, v) in edges {
        disjoint_set.union(&u, &v);
        if disjoint_set.sets() == 1 {
            return nodes[u].x * nodes[v].x;
        }
    }

    unreachable!()
}

fn main() {
    let input = read_input_file!();
    let nodes = parse_input(&input);

    println!("{}", star1(&nodes));
    println!("{}", star2(&nodes));
}
