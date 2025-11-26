use std::{
    io::{BufRead, Cursor},
    str::FromStr,
};

pub struct Scanner<R> {
    reader: R,
    buffer: Vec<String>,
}

impl Scanner<()> {
    pub fn from_str_slice(s: &str) -> Scanner<Cursor<&str>> {
        Scanner::new(Cursor::new(s))
    }
}

impl<R: BufRead> Scanner<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            buffer: Vec::new(),
        }
    }
    pub fn token<T: FromStr>(&mut self) -> T {
        loop {
            if let Some(token) = self.buffer.pop() {
                return token.parse().ok().expect("Failed parse");
            }
            let mut input = String::new();
            self.reader.read_line(&mut input).expect("Failed read");
            self.buffer = input.split_whitespace().rev().map(String::from).collect();
        }
    }
}

pub fn scan_each_line(input: &str, mut f: impl FnMut(Scanner<Cursor<&str>>)) {
    input
        .trim()
        .lines()
        .for_each(|line| f(Scanner::from_str_slice(line)));
}

pub fn split_parse<T: FromStr>(s: &str) -> Vec<T> {
    s.split_whitespace()
        .map(|s| s.parse().ok().expect("Failed parse"))
        .collect()
}
