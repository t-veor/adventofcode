use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, BinaryHeap, HashMap, HashSet, VecDeque},
    hash::Hash,
};

use utils::{
    glam::IVec2,
    grid::{CharGrid, OrthoDir},
    pathfinding::dijkstra,
    read_input_file,
};

// Modified dijkstra implementation to return _all_ nodes that are part of a
// path with minimum cost.
fn modified_dijkstra<S>(
    initial: S,
    is_goal: impl Fn(&S) -> bool,
    expand: impl Fn(&S) -> Vec<(i64, S)>,
) -> Option<(i64, HashSet<S>)>
where
    S: Clone + Eq + Hash,
{
    #[derive(Clone)]
    struct DijkstraQueueItem<S> {
        state: S,
        cost: i64,
    }

    impl<S> PartialEq for DijkstraQueueItem<S> {
        fn eq(&self, other: &Self) -> bool {
            self.cost == other.cost
        }
    }

    impl<S> Eq for DijkstraQueueItem<S> {}

    impl<S> Ord for DijkstraQueueItem<S> {
        fn cmp(&self, other: &Self) -> Ordering {
            // Reversed ordering so that the max-heap binary-heap becomes a min-heap
            other.cost.cmp(&self.cost)
        }
    }

    impl<S> PartialOrd for DijkstraQueueItem<S> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    struct DijkstraRecord<S> {
        come_from: Vec<S>,
        best_cost: i64,
    }

    let mut expanded = HashSet::new();
    let mut state_data: HashMap<S, DijkstraRecord<S>> = HashMap::new();
    let mut priority_queue = BinaryHeap::new();

    priority_queue.push(DijkstraQueueItem {
        state: initial.clone(),
        cost: 0,
    });
    // Set cost and come-from of the start state
    state_data.insert(
        initial,
        DijkstraRecord {
            come_from: vec![],
            best_cost: 0,
        },
    );

    let mut found_goal = None;
    let mut best_cost = None;
    while let Some(DijkstraQueueItem { state, cost }) = priority_queue.pop() {
        if !expanded.insert(state.clone()) {
            continue;
        }

        if is_goal(&state) {
            found_goal = Some(state.clone());
            best_cost = Some(cost);
        }

        // Only break once we start pulling items from the priority queue that
        // have a higher cost.
        match best_cost {
            Some(best_cost) if cost > best_cost => break,
            _ => (),
        }

        for (weight, next_state) in expand(&state) {
            let next_cost = cost + weight;

            let next_state_entry = state_data.entry(next_state.clone());

            let better_path_found = match next_state_entry {
                Entry::Occupied(mut entry) => {
                    if next_cost < entry.get().best_cost {
                        entry.insert(DijkstraRecord {
                            come_from: vec![state.clone()],
                            best_cost: next_cost,
                        });
                        true
                    } else if next_cost == entry.get().best_cost {
                        entry.get_mut().come_from.push(state.clone());
                        false
                    } else {
                        false
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(DijkstraRecord {
                        come_from: vec![state.clone()],
                        best_cost: next_cost,
                    });
                    true
                }
            };

            if better_path_found {
                priority_queue.push(DijkstraQueueItem {
                    state: next_state,
                    cost: next_cost,
                })
            }
        }
    }

    let end_state = found_goal?;
    let best_cost = best_cost?;

    // BFS backwards through the best cost tree
    let mut seen = HashSet::new();
    let mut frontier = VecDeque::new();
    frontier.push_back(end_state);

    while let Some(state) = frontier.pop_front() {
        if !seen.insert(state.clone()) {
            continue;
        }

        let come_from = state_data
            .get(&state)
            .map(|entry| entry.come_from.as_slice())
            .unwrap_or_default();
        for prev_state in come_from {
            frontier.push_back(prev_state.clone());
        }
    }

    Some((best_cost, seen))
}

fn parse_input(input: &str) -> (CharGrid, IVec2, IVec2) {
    let mut start_pos = None;
    let mut end_pos = None;

    let grid = CharGrid::read_from_str(input.trim(), |pos, c| match c {
        'S' => {
            start_pos = Some(pos);
            '.'
        }
        'E' => {
            end_pos = Some(pos);
            '.'
        }
        _ => c,
    })
    .unwrap();

    (grid, start_pos.unwrap(), end_pos.unwrap())
}

fn star1(grid: &CharGrid, start_pos: IVec2, end_pos: IVec2) -> i64 {
    dijkstra(
        (start_pos, OrthoDir::East),
        |&(pos, _dir)| pos == end_pos,
        |&(pos, dir)| {
            let mut children = Vec::with_capacity(3);

            // Turn left
            children.push((1000, (pos, dir.rotate_ccw())));
            // Turn right
            children.push((1000, (pos, dir.rotate_cw())));
            // Move forward if possible
            let forward = dir.step(pos);
            if let Some('.') = grid.get(forward) {
                children.push((1, (forward, dir)));
            }

            children
        },
    )
    .unwrap()
    .0
}

fn star2(grid: &CharGrid, start_pos: IVec2, end_pos: IVec2) -> usize {
    let states_on_best_paths = modified_dijkstra(
        (start_pos, OrthoDir::East),
        |&(pos, _dir)| pos == end_pos,
        |&(pos, dir)| {
            let mut children = Vec::with_capacity(3);

            // Turn left
            children.push((1000, (pos, dir.rotate_ccw())));
            // Turn right
            children.push((1000, (pos, dir.rotate_cw())));
            // Move forward if possible
            let forward = dir.step(pos);
            if let Some('.') = grid.get(forward) {
                children.push((1, (forward, dir)));
            }

            children
        },
    )
    .unwrap()
    .1;

    let squares_on_best_paths: HashSet<_> = states_on_best_paths
        .into_iter()
        .map(|(pos, _dir)| pos)
        .collect();

    squares_on_best_paths.len()
}

fn main() {
    let input = read_input_file!();
    let (grid, start_pos, end_pos) = parse_input(&input);

    println!("{}", star1(&grid, start_pos, end_pos));
    println!("{}", star2(&grid, start_pos, end_pos));
}
