use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::hash::Hash;

pub fn dijkstra<S>(
    initial: S,
    is_goal: impl Fn(&S) -> bool,
    expand: impl Fn(&S) -> Vec<(S, i64)>,
) -> Option<(i64, Vec<S>)>
where
    S: Clone + Eq + Hash,
{
    #[derive(Debug, Clone)]
    struct DijkstraState<S> {
        state: S,
        cost: i64,
    }

    impl<S> PartialEq for DijkstraState<S> {
        fn eq(&self, other: &Self) -> bool {
            self.cost == other.cost
        }
    }

    impl<S> Eq for DijkstraState<S> {}

    impl<S> Ord for DijkstraState<S> {
        fn cmp(&self, other: &Self) -> Ordering {
            // Reversed ordering so that the max-heap binary-heap becomes a min-heap
            other.cost.cmp(&self.cost)
        }
    }

    impl<S> PartialOrd for DijkstraState<S> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    #[derive(Debug)]
    struct StateData<S> {
        come_from: Option<S>,
        best_cost: Option<i64>,
    }

    impl<S> Default for StateData<S> {
        fn default() -> Self {
            Self {
                come_from: None,
                best_cost: None,
            }
        }
    }

    let mut expanded = HashSet::new();
    let mut state_data: HashMap<S, StateData<S>> = HashMap::new();
    let mut priority_queue = BinaryHeap::new();

    state_data.insert(
        initial.clone(),
        StateData {
            come_from: None,
            best_cost: Some(0),
        },
    );
    priority_queue.push(DijkstraState {
        state: initial,
        cost: 0,
    });

    let mut found_goal = None;
    while let Some(DijkstraState { state, cost }) = priority_queue.pop() {
        if !expanded.insert(state.clone()) {
            continue;
        }

        if is_goal(&state) {
            found_goal = Some(state);
            break;
        }

        for (next_state, weight) in expand(&state) {
            let next_cost = cost + weight;

            let next_state_entry = state_data.entry(next_state.clone()).or_default();
            if next_state_entry.best_cost.is_none()
                || next_cost < next_state_entry.best_cost.unwrap()
            {
                next_state_entry.best_cost = Some(next_cost);
                next_state_entry.come_from = Some(state.clone());

                priority_queue.push(DijkstraState {
                    state: next_state,
                    cost: next_cost,
                })
            }
        }
    }

    found_goal.map(|state| {
        let mut path = Vec::new();
        let entry = state_data.get(&state).unwrap();

        path.push(state);

        let mut come_from = entry.come_from.clone();
        while let Some(prev) = come_from {
            let prev_entry = state_data.get(&prev).unwrap();
            come_from = prev_entry.come_from.clone();
            path.push(prev);
        }

        path.reverse();
        (entry.best_cost.unwrap(), path)
    })
}
