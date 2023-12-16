use std::{collections::HashMap, hash::Hash};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CycleInfo<S> {
    pub start: u64,
    pub length: u64,

    pub initial_element: S,
    pub first_element_in_cycle: S,
}

pub fn floyd<S>(initial_element: S, next: impl Fn(S) -> S) -> CycleInfo<S>
where
    S: Clone + Eq,
{
    let mut tortoise = next(initial_element.clone());
    let mut hare = next(tortoise.clone());

    while tortoise != hare {
        tortoise = next(tortoise);
        hare = next(next(hare));
    }

    let mut mu = 0;
    tortoise = initial_element.clone();
    while tortoise != hare {
        tortoise = next(tortoise);
        hare = next(hare);
        mu += 1;
    }

    let mut lambda = 1;
    hare = next(tortoise.clone());
    while tortoise != hare {
        hare = next(hare);
        lambda += 1;
    }

    CycleInfo {
        start: mu,
        length: lambda,

        initial_element,
        first_element_in_cycle: tortoise,
    }
}

pub fn brent<S>(initial_element: S, next: impl Fn(S) -> S) -> CycleInfo<S>
where
    S: Clone + Eq,
{
    let mut power = 1;
    let mut lambda = 1;

    let mut tortoise = initial_element.clone();
    let mut hare = next(initial_element.clone());

    while tortoise != hare {
        if power == lambda {
            tortoise = hare.clone();
            power *= 2;
            lambda = 0;
        }

        hare = next(hare);
        lambda += 1;
    }

    tortoise = initial_element.clone();
    hare = initial_element.clone();

    for _ in 0..lambda {
        hare = next(hare);
    }

    let mut mu = 0;
    while tortoise != hare {
        tortoise = next(tortoise);
        hare = next(hare);
        mu += 1;
    }

    CycleInfo {
        start: mu,
        length: lambda,

        initial_element,
        first_element_in_cycle: tortoise,
    }
}

pub fn hashmap<S>(initial_element: S, next: impl Fn(S) -> S) -> CycleInfo<S>
where
    S: Clone + Eq + Hash,
{
    let mut seen_states = HashMap::new();
    seen_states.insert(initial_element.clone(), 0);

    let mut curr = initial_element.clone();
    for curr_cycles in 1.. {
        curr = next(curr);

        if let Some(prev_cycles) = seen_states.insert(curr.clone(), curr_cycles) {
            let length = curr_cycles - prev_cycles;
            return CycleInfo {
                start: prev_cycles,
                length,

                initial_element,
                first_element_in_cycle: curr,
            };
        }
    }

    unreachable!()
}

pub fn apply_iterations_using_cycle_skip<S>(
    cycle_info: CycleInfo<S>,
    iterations: u64,
    next: impl Fn(S) -> S,
) -> S {
    let (mut curr, additional_iterations) = if iterations < cycle_info.start {
        (cycle_info.initial_element, iterations)
    } else {
        (
            cycle_info.first_element_in_cycle,
            (iterations - cycle_info.start) % cycle_info.length,
        )
    };

    for _ in 0..additional_iterations {
        curr = next(curr);
    }

    curr
}
