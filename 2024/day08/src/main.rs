use std::collections::{HashMap, HashSet};

use utils::{
    discrete_math::gcd,
    glam::{ivec2, IVec2},
    read_input_file,
};

struct AntennaMap {
    by_frequency: HashMap<char, Vec<IVec2>>,
    map_size: (i32, i32),
}

impl AntennaMap {
    fn in_bounds(&self, pos: IVec2) -> bool {
        let (width, height) = self.map_size;
        (0..width).contains(&pos.x) && (0..height).contains(&pos.y)
    }
}

fn parse_input(input: &str) -> AntennaMap {
    let mut antennae_by_frequency: HashMap<char, Vec<IVec2>> = HashMap::new();

    let mut width = 0;
    let mut height = 0;
    for (y, line) in input.trim().lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c != '.' {
                antennae_by_frequency
                    .entry(c)
                    .or_default()
                    .push(ivec2(x as _, y as _));
            }
            width = width.max(x + 1);
        }
        height += 1;
    }

    AntennaMap {
        by_frequency: antennae_by_frequency,
        map_size: (width as i32, height),
    }
}

fn find_all_antinodes(
    antennae: &AntennaMap,
    antenna1: IVec2,
    antenna2: IVec2,
    mut cb: impl FnMut(IVec2),
) {
    let delta = antenna2 - antenna1;

    // Reduce delta to lowest form
    // (looks like this isn't needed for my input??)
    let common_divisor = gcd(delta.x as _, delta.y as _) as i32;
    let delta = delta / common_divisor;

    // Series of antinodes going backwards from antenna1
    let mut pos = antenna1;
    while antennae.in_bounds(pos) {
        cb(pos);
        pos -= delta;
    }

    // Series of antinodes going forwards from antenna1
    // (Can skip antenna1 itself since we already covered it earlier)
    let mut pos = antenna1 + delta;
    while antennae.in_bounds(pos) {
        cb(pos);
        pos += delta;
    }
}

fn star1(antennae: &AntennaMap) -> usize {
    let mut antinodes = HashSet::new();

    for antennae_for_freq in antennae.by_frequency.values() {
        for i in 0..antennae_for_freq.len() {
            for j in i + 1..antennae_for_freq.len() {
                let antenna1 = antennae_for_freq[i];
                let antenna2 = antennae_for_freq[j];

                let delta = antenna2 - antenna1;

                let antinode1 = antenna1 - delta;
                let antinode2 = antenna2 + delta;

                antinodes.insert(antinode1);
                antinodes.insert(antinode2);
            }
        }
    }

    antinodes
        .iter()
        .filter(|coord| antennae.in_bounds(**coord))
        .count()
}

fn star2(antennae: &AntennaMap) -> usize {
    let mut antinodes = HashSet::new();

    for antennae_for_freq in antennae.by_frequency.values() {
        for i in 0..antennae_for_freq.len() {
            for j in i + 1..antennae_for_freq.len() {
                find_all_antinodes(
                    antennae,
                    antennae_for_freq[i],
                    antennae_for_freq[j],
                    |loc| {
                        antinodes.insert(loc);
                    },
                );
            }
        }
    }

    antinodes.len()
}

fn main() {
    let input = read_input_file!();
    let antennae = parse_input(&input);

    println!("{}", star1(&antennae));
    println!("{}", star2(&antennae));
}
