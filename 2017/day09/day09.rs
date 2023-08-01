#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

#[derive(Debug)]
enum StreamData<'a> {
    Group { contents: Vec<StreamData<'a>> },
    Garbage { range: &'a [u8] },
}

impl<'a> StreamData<'a> {
    fn new(data: &'a str) -> Self {
        let data = data.as_bytes();

        let (this, remainder) = Self::parse(data).expect("Not a valid group");

        if !remainder.is_empty() {
            unreachable!("Not a valid group")
        }

        this
    }

    fn parse(data: &'a [u8]) -> Option<(Self, &'a [u8])> {
        Self::parse_group(data).or_else(|| Self::parse_garbage(data))
    }

    fn parse_group(mut data: &'a [u8]) -> Option<(Self, &'a [u8])> {
        if data.get(0) != Some(&b'{') {
            return None;
        }
        data = &data[1..];

        let mut contents = Vec::new();

        while let Some((child, remainder)) = Self::parse(data) {
            contents.push(child);
            data = remainder;

            if data.get(0) != Some(&b',') {
                break;
            }

            data = &data[1..];
        }

        if data.get(0) != Some(&b'}') {
            return None;
        }

        Some((Self::Group { contents }, &data[1..]))
    }

    fn parse_garbage(data: &'a [u8]) -> Option<(Self, &'a [u8])> {
        if data.get(0) != Some(&b'<') {
            return None;
        }

        let mut i = 1;
        while i <= data.len() {
            if data[i] == b'!' {
                i += 1;
            } else if data[i] == b'>' {
                return Some((
                    Self::Garbage {
                        range: &data[..i + 1],
                    },
                    &data[i + 1..],
                ));
            }

            i += 1;
        }

        None
    }

    fn score_recursive(&self, previous: i32) -> i32 {
        match self {
            StreamData::Group { contents } => {
                previous
                    + 1
                    + contents
                        .iter()
                        .map(|s| s.score_recursive(previous + 1))
                        .sum::<i32>()
            }
            StreamData::Garbage { .. } => 0,
        }
    }

    fn score(&self) -> i32 {
        self.score_recursive(0)
    }

    fn count_garbage(&self) -> i32 {
        match self {
            StreamData::Group { contents } => contents.iter().map(Self::count_garbage).sum(),
            StreamData::Garbage { range } => {
                let range = &range[1..range.len() - 1];
                let mut noncancelled = 0;
                let mut i = 0;
                while i < range.len() {
                    if range[i] == b'!' {
                        i += 1;
                    } else {
                        noncancelled += 1;
                    }
                    i += 1;
                }

                return noncancelled;
            }
        }
    }
}

fn parse_input(input: &str) -> StreamData<'_> {
    StreamData::new(input.trim())
}

fn star1(input: &StreamData<'_>) -> i32 {
    input.score()
}

fn star2(input: &StreamData<'_>) -> i32 {
    input.count_garbage()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
