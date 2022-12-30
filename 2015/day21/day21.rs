#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

#[derive(Debug, Clone, Copy, Default)]
struct Item {
    cost: i32,
    damage: i32,
    armor: i32,
}

fn parse_items(s: &str) -> Vec<Item> {
    s.lines()
        .map(|line| {
            let split: Vec<_> = line.split_ascii_whitespace().collect();
            let (cost, damage, armor) = match split[..] {
                [.., cost, damage, armor] => (cost, damage, armor),
                _ => unreachable!(),
            };
            Item {
                cost: cost.parse().unwrap(),
                damage: damage.parse().unwrap(),
                armor: armor.parse().unwrap(),
            }
        })
        .collect()
}

const WEAPONS: &str = "Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0";

const ARMOR: &str = "Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5";

const RINGS: &str = "Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3";

fn shop_weapons() -> Vec<Item> {
    parse_items(WEAPONS)
}

fn shop_armor() -> Vec<Item> {
    parse_items(ARMOR)
}

fn shop_rings() -> Vec<Item> {
    parse_items(RINGS)
}

#[derive(Debug, Clone, Copy)]
struct Stats {
    hp: i32,
    damage: i32,
    armor: i32,
}

const PLAYER_HP: i32 = 100;

fn parse_input(input: String) -> Stats {
    let stats: Vec<i32> = input
        .lines()
        .map(|line| line.split_once(": ").unwrap().1.parse().unwrap())
        .collect();

    Stats {
        hp: stats[0],
        damage: stats[1],
        armor: stats[2],
    }
}

fn player_wins(player_stats: Stats, boss_stats: Stats) -> bool {
    let player_dmg_per_turn = (boss_stats.damage - player_stats.armor).max(1);
    let boss_dmg_per_turn = (player_stats.damage - boss_stats.armor).max(1);

    let ceil_div = |a, b| (a + b - 1) / b;

    let turns_to_kill_player = ceil_div(player_stats.hp, player_dmg_per_turn);
    let turns_to_kill_boss = ceil_div(boss_stats.hp, boss_dmg_per_turn);

    turns_to_kill_player >= turns_to_kill_boss
}

fn armor_combos() -> Vec<Item> {
    let mut armor = shop_armor();
    armor.push(Item::default());
    armor
}

fn ring_combos() -> Vec<Item> {
    let rings = shop_rings();
    let mut combos = Vec::new();

    // 0 rings
    combos.push(Item::default());

    // 1 ring
    for &ring in rings.iter() {
        combos.push(ring);
    }

    // 2 rings
    for i in 0..rings.len() {
        for j in i + 1..rings.len() {
            let a = rings[i];
            let b = rings[j];
            combos.push(Item {
                cost: a.cost + b.cost,
                damage: a.damage + b.damage,
                armor: a.armor + b.armor,
            })
        }
    }

    combos
}

fn star1(input: &Stats) -> i32 {
    let boss_stats = *input;
    let weapons = shop_weapons();
    let armor = armor_combos();
    let rings = ring_combos();

    let mut min_cost = i32::MAX;
    for weapon in weapons.iter() {
        for armor in armor.iter() {
            for ring in rings.iter() {
                let cost = weapon.cost + armor.cost + ring.cost;
                let player_stats = Stats {
                    hp: PLAYER_HP,
                    damage: weapon.damage + armor.damage + ring.damage,
                    armor: weapon.armor + armor.armor + ring.armor,
                };

                if cost < min_cost && player_wins(player_stats, boss_stats) {
                    min_cost = cost;
                }
            }
        }
    }

    min_cost
}

fn star2(input: &Stats) -> i32 {
    let boss_stats = *input;
    let weapons = shop_weapons();
    let armor = armor_combos();
    let rings = ring_combos();

    let mut max_cost = i32::MIN;
    for weapon in weapons.iter() {
        for armor in armor.iter() {
            for ring in rings.iter() {
                let cost = weapon.cost + armor.cost + ring.cost;
                let player_stats = Stats {
                    hp: PLAYER_HP,
                    damage: weapon.damage + armor.damage + ring.damage,
                    armor: weapon.armor + armor.armor + ring.armor,
                };

                if cost > max_cost && !player_wins(player_stats, boss_stats) {
                    max_cost = cost;
                }
            }
        }
    }

    max_cost
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
