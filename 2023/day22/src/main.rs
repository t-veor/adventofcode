use std::collections::{BTreeMap, HashMap, HashSet};

use itertools::Itertools;
use utils::read_input_file;

#[derive(Debug)]
struct Brick {
    start: (i32, i32, i32),
    end: (i32, i32, i32),
}

type BrickIdxDag = HashMap<usize, Vec<usize>>;
type LayerMap = BTreeMap<i32, HashMap<(i32, i32), usize>>;

impl Brick {
    fn height(&self) -> i32 {
        self.start.2.abs_diff(self.end.2) as i32 + 1
    }

    fn min_z(&self) -> i32 {
        self.start.2.min(self.end.2)
    }

    fn shadow(&self) -> Vec<(i32, i32)> {
        let (start, end) = (self.start, self.end);
        if start.0 != end.0 {
            let min_x = start.0.min(end.0);
            let max_x = start.0.max(end.0);
            (min_x..=max_x).map(|x| (x, start.1)).collect()
        } else if start.1 != end.1 {
            let min_y = start.1.min(end.1);
            let max_y = start.1.max(end.1);
            (min_y..=max_y).map(|y| (start.0, y)).collect()
        } else {
            vec![(start.0, start.1)]
        }
    }
}

fn parse_input(input: &str) -> Vec<Brick> {
    input
        .lines()
        .map(|line| {
            let (start, end) = line.split_once('~').unwrap();
            let start = start
                .split(',')
                .map(|i| i.parse().unwrap())
                .next_tuple()
                .unwrap();
            let end = end
                .split(',')
                .map(|i| i.parse().unwrap())
                .next_tuple()
                .unwrap();

            Brick { start, end }
        })
        .collect()
}

fn stack_bricks(bricks: &[Brick]) -> BrickIdxDag {
    let mut brick_dag = BrickIdxDag::new();
    let mut layers = LayerMap::new();

    'outer: for (brick_idx, brick) in bricks.iter().enumerate() {
        let shadow = brick.shadow();
        let initial_height = brick.min_z();

        let mut layers_range = layers.range_mut(..initial_height);
        while let Some((&layer_height, layer)) = layers_range.next_back() {
            let intersections: Vec<usize> = shadow
                .iter()
                .filter_map(|pos| layer.get(pos))
                .copied()
                .collect();

            if !intersections.is_empty() {
                // Brick found a resting place.
                // Add shadow to the current layer height + brick height
                let dest_layer = layers.entry(layer_height + brick.height()).or_default();
                dest_layer.extend(shadow.into_iter().map(|pos| (pos, brick_idx)));

                // Update DAG
                for lower_brick_idx in intersections {
                    let children = brick_dag.entry(lower_brick_idx).or_default();
                    if !children.contains(&brick_idx) {
                        children.push(brick_idx);
                    }
                }

                continue 'outer;
            }
        }

        // Reaching here means that no layer was found to have an overlap with
        // the shadow. Therefore the brick should just sit on the ground
        let dest_layer = layers.entry(brick.height()).or_default();
        dest_layer.extend(shadow.into_iter().map(|pos| (pos, brick_idx)));
    }

    brick_dag
}

fn invert_dag(dag: &BrickIdxDag) -> BrickIdxDag {
    let mut inverted_dag = BrickIdxDag::new();

    for (&parent, children) in dag {
        for &child in children {
            let parents = inverted_dag.entry(child).or_default();
            if !parents.contains(&parent) {
                parents.push(parent);
            }
        }
    }

    inverted_dag
}

fn compute_dominators(num_bricks: usize, dag: &BrickIdxDag) -> HashMap<usize, HashSet<usize>> {
    let inverted_dag = invert_dag(dag);
    let mut dominator_sets = HashMap::new();

    fn get_dominating_set(
        idx: usize,
        inverted_dag: &BrickIdxDag,
        dominator_sets: &mut HashMap<usize, HashSet<usize>>,
    ) -> HashSet<usize> {
        if let Some(set) = dominator_sets.get(&idx) {
            return set.clone();
        }

        let mut set = HashSet::from([idx]);

        if let Some(parents) = inverted_dag.get(&idx) {
            if let Some(intersection) = parents
                .iter()
                .map(|&parent_idx| get_dominating_set(parent_idx, inverted_dag, dominator_sets))
                .reduce(|a, b| a.intersection(&b).copied().collect())
            {
                set.extend(intersection.into_iter());
            }
        }

        dominator_sets.insert(idx, set.clone());
        set
    }

    for i in 0..num_bricks {
        get_dominating_set(i, &inverted_dag, &mut dominator_sets);
    }

    dominator_sets
}

fn star1(num_bricks: usize, brick_dag: &BrickIdxDag) -> usize {
    let mut supporter_count = vec![0; num_bricks];
    for supported in brick_dag.values() {
        for &supported_brick in supported {
            supporter_count[supported_brick] += 1;
        }
    }

    (0..num_bricks)
        .filter(|brick_idx| {
            let supported = match brick_dag.get(brick_idx) {
                Some(supported) => supported,
                None => {
                    // No bricks supported, safe to remove
                    return true;
                }
            };

            // Can remove 1 from all supported bricks without any dropping to 0
            supported
                .iter()
                .all(|&supported_brick| supporter_count[supported_brick] > 1)
        })
        .count()
}

fn star2(num_bricks: usize, brick_dag: &BrickIdxDag) -> usize {
    let doms = compute_dominators(num_bricks, brick_dag);
    doms.values().map(|set| set.len() - 1).sum()
}

fn main() {
    let input = read_input_file!();
    let mut bricks = parse_input(&input);

    // Sort bricks by min-z
    bricks.sort_by_key(|brick| brick.min_z());
    let brick_dag = stack_bricks(&bricks);

    println!("{}", star1(bricks.len(), &brick_dag));
    println!("{}", star2(bricks.len(), &brick_dag));
}
