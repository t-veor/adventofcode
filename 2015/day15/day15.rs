#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

// Weird, this is almost a linear programming problem, but it's not quite as the
// final cost function is non-linear. Still, searching through all the possible
// combinations is O(100^(n-1)), and n = 4 in the input means it's not actually
// very bad to just generate all combinations and search through them.

#[derive(Debug, Clone)]
struct Ingredient {
    capacity: i32,
    durability: i32,
    flavor: i32,
    texture: i32,
    calories: i32,
}

fn parse_input(input: String) -> Vec<Ingredient> {
    fn extract(line: &str) -> Ingredient {
        let (_, stats) = line.split_once(": ").unwrap();
        let stats: Vec<i32> = stats
            .split(", ")
            .map(|i| i.split_once(' ').unwrap().1.parse().unwrap())
            .collect();
        Ingredient {
            capacity: stats[0],
            durability: stats[1],
            flavor: stats[2],
            texture: stats[3],
            calories: stats[4],
        }
    }

    input.lines().map(extract).collect()
}

struct RecipeIterator {
    limit: i32,
    curr_state: Vec<i32>,
    done: bool,
}

impl RecipeIterator {
    fn new(n: usize, limit: i32) -> Self {
        Self {
            limit,
            curr_state: vec![0; n.saturating_sub(1)],
            done: n == 0,
        }
    }
}

impl Iterator for RecipeIterator {
    type Item = Vec<i32>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        if self.curr_state.is_empty() {
            self.done = true;
            return Some(vec![self.limit]);
        }

        let mut result = Vec::with_capacity(self.curr_state.len() + 1);
        result.push(self.curr_state[0]);
        for i in 1..self.curr_state.len() {
            result.push(self.curr_state[i] - self.curr_state[i - 1]);
        }
        result.push(self.limit - self.curr_state.last().unwrap());

        for i in (0..self.curr_state.len()).rev() {
            self.curr_state[i] += 1;
            if self.curr_state[i] <= self.limit {
                for j in i + 1..self.curr_state.len() {
                    self.curr_state[j] = self.curr_state[i];
                }
                return Some(result);
            }
        }

        self.done = true;
        Some(result)
    }
}

fn get_property<F>(ingredients: &[Ingredient], amounts: &[i32], getter: F) -> i32
where
    F: Fn(&Ingredient) -> i32,
{
    ingredients
        .iter()
        .zip(amounts)
        .map(|(ingredient, amount)| getter(ingredient) * amount)
        .sum::<i32>()
        .max(0)
}

fn star1(input: &[Ingredient]) -> i32 {
    let score_recipe = |amounts: &[i32]| {
        let capacity = get_property(input, amounts, |x| x.capacity);
        let durability = get_property(input, amounts, |x| x.durability);
        let flavor = get_property(input, amounts, |x| x.flavor);
        let texture = get_property(input, amounts, |x| x.texture);

        capacity * durability * flavor * texture
    };

    RecipeIterator::new(input.len(), 100)
        .map(|recipe| score_recipe(&recipe))
        .max()
        .unwrap()
}

fn star2(input: &[Ingredient]) -> i32 {
    let score_recipe = |amounts: &[i32]| {
        let calories = get_property(input, amounts, |x| x.calories);

        if calories == 500 {
            let capacity = get_property(input, amounts, |x| x.capacity);
            let durability = get_property(input, amounts, |x| x.durability);
            let flavor = get_property(input, amounts, |x| x.flavor);
            let texture = get_property(input, amounts, |x| x.texture);
            capacity * durability * flavor * texture
        } else {
            0
        }
    };

    RecipeIterator::new(input.len(), 100)
        .map(|recipe| score_recipe(&recipe))
        .max()
        .unwrap()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
