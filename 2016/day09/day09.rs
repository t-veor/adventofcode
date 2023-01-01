#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: &str) -> &str {
    input.trim()
}

fn decompress(input: &str) -> String {
    let mut bytes = input.as_bytes();
    let mut result = Vec::new();

    while !bytes.is_empty() {
        let next_marker = bytes.iter().position(|&c| c == b'(');
        if let Some(marker_start) = next_marker {
            result.extend_from_slice(&bytes[0..marker_start]);
            let marker_end = bytes
                .iter()
                .skip(marker_start)
                .position(|&c| c == b')')
                .unwrap()
                + marker_start;

            let marker = std::str::from_utf8(&bytes[marker_start + 1..marker_end]).unwrap();
            let (length, repeats) = marker.split_once('x').unwrap();
            let length = length.parse::<usize>().unwrap();
            let repeats = repeats.parse::<usize>().unwrap();

            let repeated_bytes = &bytes[marker_end + 1..marker_end + 1 + length];
            for _ in 0..repeats {
                result.extend_from_slice(repeated_bytes);
            }
            bytes = &bytes[marker_end + 1 + length..];
        } else {
            // End of input
            result.extend_from_slice(bytes);
            bytes = &[];
        }
    }

    String::from_utf8(result).unwrap()
}

fn star1(input: &str) -> usize {
    decompress(input).len()
}

fn decompress_v2(input: &str) -> u64 {
    fn recurse(mut input: &[u8]) -> u64 {
        let mut result = 0;

        while !input.is_empty() {
            let next_marker = input.iter().position(|&c| c == b'(');
            if let Some(marker_start) = next_marker {
                result += marker_start as u64;

                let marker_end = input
                    .iter()
                    .skip(marker_start)
                    .position(|&c| c == b')')
                    .unwrap()
                    + marker_start;

                let marker = std::str::from_utf8(&input[marker_start + 1..marker_end]).unwrap();
                let (length, repeats) = marker.split_once('x').unwrap();
                let length = length.parse::<usize>().unwrap();
                let repeats = repeats.parse::<u64>().unwrap();

                let repeated_bytes = &input[marker_end + 1..marker_end + 1 + length];
                result += repeats * recurse(repeated_bytes);

                input = &input[marker_end + 1 + length..];
            } else {
                // End of input
                result += input.len() as u64;
                input = &[];
            }
        }

        result
    }

    recurse(input.as_bytes())
}

fn star2(input: &str) -> u64 {
    decompress_v2(input)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
