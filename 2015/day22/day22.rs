#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{cmp::Reverse, collections::BinaryHeap};

#[derive(Debug, Clone, Eq)]
struct State {
    is_player: bool,
    player_hp: i32,
    player_mana: i32,

    boss_hp: i32,
    boss_dmg: i32,

    mana_spent: i32,

    shield_turns: i32,
    poison_turns: i32,
    recharge_turns: i32,
}

impl State {
    fn initial_state(boss_hp: i32, boss_dmg: i32) -> Self {
        Self {
            is_player: true,
            player_hp: 50,
            player_mana: 500,
            boss_hp,
            boss_dmg,
            mana_spent: 0,
            shield_turns: 0,
            poison_turns: 0,
            recharge_turns: 0,
        }
    }

    fn is_goal(&self) -> bool {
        self.boss_hp <= 0
    }

    fn expand(&self, hard_mode: bool) -> Vec<Self> {
        if self.is_goal() {
            return vec![];
        }

        let mut result = Vec::with_capacity(5);

        let mut player_armor = 0;
        let state_after_effects = {
            let mut new_state = self.clone();

            if hard_mode && new_state.is_player {
                new_state.player_hp -= 1;
                if new_state.player_hp <= 0 {
                    return vec![];
                }
            }

            if new_state.shield_turns > 0 {
                new_state.shield_turns -= 1;
                player_armor = 7;
            }
            if new_state.poison_turns > 0 {
                new_state.poison_turns -= 1;
                new_state.boss_hp -= 3;
                if new_state.boss_hp <= 0 {
                    // Boss has died, instantly produce winning state!
                    return vec![new_state];
                }
            }
            if new_state.recharge_turns > 0 {
                new_state.recharge_turns -= 1;
                new_state.player_mana += 101;
            }

            new_state.is_player = !new_state.is_player;
            new_state
        };

        if self.is_player {
            result.extend(
                [
                    state_after_effects.magic_missile(),
                    state_after_effects.drain(),
                    state_after_effects.shield(),
                    state_after_effects.poison(),
                    state_after_effects.recharge(),
                ]
                .into_iter()
                .filter_map(|x| x),
            );
        } else {
            let boss_attack = (state_after_effects.boss_dmg - player_armor).max(1);
            let player_hp = state_after_effects.player_hp - boss_attack;
            if player_hp > 0 {
                result.push(Self {
                    player_hp,
                    ..state_after_effects
                });
            }
        }

        result
    }

    fn spend_mana(&self, mana: i32) -> Option<Self> {
        (self.player_mana >= mana).then(|| Self {
            player_mana: self.player_mana - mana,
            mana_spent: self.mana_spent + mana,
            ..self.clone()
        })
    }

    fn magic_missile(&self) -> Option<Self> {
        self.spend_mana(53).map(|state| Self {
            boss_hp: state.boss_hp - 4,
            ..state
        })
    }

    fn drain(&self) -> Option<Self> {
        self.spend_mana(73).map(|state| Self {
            boss_hp: state.boss_hp - 2,
            player_hp: state.player_hp + 2,
            ..state
        })
    }

    fn shield(&self) -> Option<Self> {
        self.spend_mana(113).and_then(|state| {
            (state.shield_turns == 0).then(|| Self {
                shield_turns: 6,
                ..state
            })
        })
    }

    fn poison(&self) -> Option<Self> {
        self.spend_mana(173).and_then(|state| {
            (state.poison_turns == 0).then(|| Self {
                poison_turns: 6,
                ..state
            })
        })
    }

    fn recharge(&self) -> Option<Self> {
        self.spend_mana(229).and_then(|state| {
            (state.recharge_turns == 0).then(|| Self {
                recharge_turns: 5,
                ..state
            })
        })
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.mana_spent.cmp(&other.mana_spent)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.mana_spent == other.mana_spent
    }
}

fn parse_input(input: String) -> State {
    let stats: Vec<i32> = input
        .lines()
        .map(|line| line.split_once(": ").unwrap().1.parse().unwrap())
        .collect();

    State::initial_state(stats[0], stats[1])
}

fn best_first_search<T>(
    initial_state: T,
    is_goal: impl Fn(&T) -> bool,
    expand: impl Fn(&T) -> Vec<T>,
) -> Option<T>
where
    T: Ord + Clone,
{
    let mut queue = BinaryHeap::new();
    queue.push(initial_state);

    while let Some(state) = queue.pop() {
        if is_goal(&state) {
            return Some(state);
        }

        for next_state in expand(&state) {
            queue.push(next_state);
        }
    }

    None
}

fn star1(input: &State) -> i32 {
    best_first_search(
        Reverse(input.clone()),
        |Reverse(state)| state.is_goal(),
        |Reverse(state)| state.expand(false).into_iter().map(Reverse).collect(),
    )
    .expect("cannot win battle")
    .0
    .mana_spent
}

fn star2(input: &State) -> i32 {
    best_first_search(
        Reverse(input.clone()),
        |Reverse(state)| state.is_goal(),
        |Reverse(state)| state.expand(true).into_iter().map(Reverse).collect(),
    )
    .expect("cannot win battle")
    .0
    .mana_spent
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
