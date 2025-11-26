use std::cmp::Ordering;
use std::collections::hash_map::Entry;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug, Clone)]
struct AStarQueueItem<S> {
    state: S,
    cost: i64,
    priority: i64,
}

impl<S> PartialEq for AStarQueueItem<S> {
    fn eq(&self, other: &Self) -> bool {
        self.priority == other.priority
    }
}

impl<S> Eq for AStarQueueItem<S> {}

impl<S> Ord for AStarQueueItem<S> {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reversed ordering so that the max-heap binary-heap becomes a min-heap
        other.priority.cmp(&self.priority)
    }
}

impl<S> PartialOrd for AStarQueueItem<S> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
struct AStarRecord<S> {
    come_from: Option<S>,
    best_cost: i64,
}

pub fn a_star<S>(
    initial: S,
    is_goal: impl Fn(&S) -> bool,
    expand: impl Fn(&S) -> Vec<(i64, S)>,
    heuristic: impl Fn(&S) -> i64,
) -> Option<(i64, Vec<S>)>
where
    S: Clone + Eq + Hash,
{
    let mut expanded = HashSet::new();
    let mut state_data: HashMap<S, AStarRecord<S>> = HashMap::new();
    let mut priority_queue = BinaryHeap::new();

    priority_queue.push(AStarQueueItem {
        state: initial.clone(),
        cost: 0,
        // Technically this is wrong - but this entry gets immediately removed
        // so we don't care that it has a dummy priority of 0
        priority: 0,
    });
    // Set cost and come-from of the start state
    state_data.insert(
        initial,
        AStarRecord {
            come_from: None,
            best_cost: 0,
        },
    );

    let mut found_goal = None;
    while let Some(AStarQueueItem { state, cost, .. }) = priority_queue.pop() {
        if !expanded.insert(state.clone()) {
            continue;
        }

        if is_goal(&state) {
            found_goal = Some(state);
            break;
        }

        for (weight, next_state) in expand(&state) {
            let next_cost = cost + weight;

            let next_state_entry = state_data.entry(next_state.clone());

            let better_path_found = match next_state_entry {
                Entry::Occupied(mut entry) => {
                    if next_cost < entry.get().best_cost {
                        entry.insert(AStarRecord {
                            come_from: Some(state.clone()),
                            best_cost: next_cost,
                        });
                        true
                    } else {
                        false
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(AStarRecord {
                        come_from: Some(state.clone()),
                        best_cost: next_cost,
                    });
                    true
                }
            };

            if better_path_found {
                let h = heuristic(&next_state);

                priority_queue.push(AStarQueueItem {
                    state: next_state,
                    cost: next_cost,
                    priority: next_cost + h,
                })
            }
        }
    }

    found_goal.map(|state| {
        let mut path = Vec::new();
        let entry = state_data.get(&state);

        path.push(state);

        let mut come_from = entry.and_then(|entry| entry.come_from.clone());
        while let Some(prev) = come_from {
            let prev_entry = state_data.get(&prev);
            come_from = prev_entry.and_then(|entry| entry.come_from.clone());
            path.push(prev);
        }

        path.reverse();
        (entry.map(|entry| entry.best_cost).unwrap_or(0), path)
    })
}

pub fn dijkstra<S>(
    initial: S,
    is_goal: impl Fn(&S) -> bool,
    expand: impl Fn(&S) -> Vec<(i64, S)>,
) -> Option<(i64, Vec<S>)>
where
    S: Clone + Eq + Hash,
{
    a_star(initial, is_goal, expand, |_| 0)
}
