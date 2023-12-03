use std::collections::HashMap;

use itertools::Itertools;
use utils::read_input_file;

#[derive(Debug)]
struct Schematic {
    numbers: Vec<PartNumber>,
    symbols: Vec<Symbol>,
}

#[derive(Debug)]
struct PartNumber {
    value: i32,
    adjacent_symbols: Vec<usize>,
}

#[derive(Debug)]
struct Symbol {
    symbol: u8,
    adjacent_numbers: Vec<usize>,
}

impl Schematic {
    fn parse(input: &str) -> Self {
        let lines: Vec<_> = input.lines().map(|line| line.as_bytes()).collect();

        let mut numbers = Vec::new();
        let mut symbols = Vec::new();

        let mut symbols_by_location = HashMap::new();

        // Collect all symbols
        for (row, line) in lines.iter().enumerate() {
            for (col, &char) in line.iter().enumerate() {
                if char != b'.' && !char.is_ascii_digit() {
                    let idx = symbols.len();
                    symbols.push(Symbol {
                        symbol: char,
                        adjacent_numbers: Vec::new(),
                    });

                    symbols_by_location.insert((row, col), idx);
                }
            }
        }

        // Collect all numbers and check for adjacent symbols
        for (row, line) in lines.iter().enumerate() {
            let groups_iter = line
                .iter()
                .enumerate()
                .group_by(|(_, char)| char.is_ascii_digit());

            for (is_digit, group) in &groups_iter {
                if !is_digit {
                    continue;
                }

                let group: Vec<_> = group.collect();
                let first_col = group.first().unwrap().0;
                let last_col = group.last().unwrap().0;

                let value = group
                    .iter()
                    .fold(0, |acc, (_, &digit)| acc * 10 + (digit - b'0') as i32);

                let idx = numbers.len();
                let mut number = PartNumber {
                    value,
                    adjacent_symbols: Vec::new(),
                };

                for r in row.saturating_sub(1)..=row + 1 {
                    for c in first_col.saturating_sub(1)..=last_col + 1 {
                        if let Some(&symbol_idx) = symbols_by_location.get(&(r, c)) {
                            number.adjacent_symbols.push(symbol_idx);
                            symbols[symbol_idx].adjacent_numbers.push(idx);
                        }
                    }
                }

                numbers.push(number);
            }
        }

        Self { numbers, symbols }
    }
}

fn star1(schematic: &Schematic) -> i32 {
    schematic
        .numbers
        .iter()
        .filter(|number| !number.adjacent_symbols.is_empty())
        .map(|number| number.value)
        .sum()
}

fn star2(schematic: &Schematic) -> i32 {
    schematic
        .symbols
        .iter()
        .filter(|symbol| symbol.symbol == b'*' && symbol.adjacent_numbers.len() == 2)
        .map(|symbol| {
            symbol
                .adjacent_numbers
                .iter()
                .map(|&idx| schematic.numbers[idx].value)
                .product::<i32>()
        })
        .sum()
}

fn main() {
    let input = read_input_file!();
    let schematic = Schematic::parse(&input);

    println!("{}", star1(&schematic));
    println!("{}", star2(&schematic));
}
