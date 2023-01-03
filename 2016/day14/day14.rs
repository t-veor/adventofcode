#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! md5 = "0.7"
//! ```

// I gotta admit, I don't really like the puzzles where you just use a hash
// function as a black box, and end up with you having to crank out a couple
// million hashes. In this case you have to crank out about ~50 million hashes
// with no way of negotiating that number down

use std::collections::VecDeque;

fn parse_input(input: &str) -> &str {
    input.trim()
}

fn digest_to_hash(digest: md5::Digest) -> [u8; 32] {
    let mut result = [0; 32];

    for i in 0..16 {
        let val = digest.0[i];
        let upper = (val & 0xf0) >> 4;
        let lower = val & 0x0f;
        result[i * 2] = upper;
        result[i * 2 + 1] = lower;
    }

    result
}

fn get_hash(salt: &str, val: u32) -> [u8; 32] {
    let digest = md5::compute(format!("{salt}{val}"));
    digest_to_hash(digest)
}

#[allow(unused)]
fn dbg_hash(hash: &[u8; 32]) -> String {
    let mut result = String::with_capacity(32);
    for i in hash {
        result.push_str(&format!("{i:x}"));
    }
    result
}

fn get_repeats(hash: &[u8; 32]) -> (Option<u8>, Vec<u8>) {
    let mut first_triple = None;
    let mut quintuples = Vec::new();
    for i in 2..32 {
        let (a, b, c) = (hash[i], hash[i - 1], hash[i - 2]);
        if a == b && a == c {
            if first_triple.is_none() {
                first_triple = Some(a);
            }

            if i >= 4 {
                let (d, e) = (hash[i - 3], hash[i - 4]);
                if a == d && a == e {
                    if !quintuples.contains(&a) {
                        quintuples.push(a);
                    }
                }
            }
        }
    }

    (first_triple, quintuples)
}

fn get_keys(salt: &str, required_count: usize, hasher: impl Fn(&str, u32) -> [u8; 32]) -> Vec<u32> {
    if required_count == 0 {
        return vec![];
    }

    let mut result = Vec::new();
    let mut queues = [(); 16].map(|_| VecDeque::new());

    for i in 0.. {
        // Remove any triples from the queues that are now too old to qualify
        for queue in queues.iter_mut() {
            loop {
                match queue.front() {
                    Some(j) if i - j > 1000 => queue.pop_front(),
                    _ => break,
                };
            }
        }

        let hash = hasher(salt, i);
        let (first_triple, quintuples) = get_repeats(&hash);

        // Found a quintuple repeat. Move all the triples for that quintuple
        // into the results list
        for quintuple_repeat in quintuples {
            let queue = &mut queues[quintuple_repeat as usize];
            while let Some(idx) = queue.pop_front() {
                match result.binary_search(&idx) {
                    Ok(_) => unreachable!(),
                    Err(insert_pos) => {
                        result.insert(insert_pos, idx);
                    }
                }
            }
        }

        // Add any triples to the triples queues
        if let Some(triple_repeat) = first_triple {
            queues[triple_repeat as usize].push_back(i);
        }

        // The results might be out of order (e.g. hash 103 might append hash 3
        // and 4, but then hash 104 might append hash 1) so to be safe we need
        // to make sure that we wait until 1000 hashes have been considered past
        // the required_count-th hash in the list
        if result.len() >= required_count && i - result[required_count - 1] > 1000 {
            break;
        }
    }

    result.truncate(required_count);
    result
}

fn star1(input: &str) -> u32 {
    let keys = get_keys(input, 64, get_hash);
    *keys.last().unwrap()
}

fn get_stretched_hash(salt: &str, val: u32) -> [u8; 32] {
    let mut digest = md5::compute(format!("{salt}{val}"));
    for _ in 0..2016 {
        // This used to be format!("{digest:x}") but it turns out that's
        // significantly slower than this
        let mut hex = digest_to_hash(digest);
        hex.iter_mut().for_each(|x| {
            if *x >= 10 {
                *x += b'a' - 10
            } else {
                *x += b'0'
            }
        });

        digest = md5::compute(hex);
    }

    digest_to_hash(digest)
}

fn star2(input: &str) -> u32 {
    let keys = get_keys(input, 64, get_stretched_hash);
    *keys.last().unwrap()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
