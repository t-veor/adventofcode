#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashMap;

#[derive(Debug, Clone)]
enum WireTerm {
    Constant(u16),
    Wire(String),
}

impl WireTerm {
    fn new(s: &str) -> Self {
        if let Ok(x) = s.parse() {
            Self::Constant(x)
        } else {
            Self::Wire(s.to_owned())
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum BinOp {
    And,
    Or,
    LShift,
    RShift,
}

#[derive(Debug, Clone)]
enum WireConnection {
    Exact(WireTerm),
    BinOp(BinOp, WireTerm, WireTerm),
    Not(WireTerm),
}

fn parse_input(input: String) -> HashMap<String, WireConnection> {
    let mut wires = HashMap::new();

    for line in input.lines() {
        let (expr, dst) = line.split_once(" -> ").unwrap();
        let expr: Vec<_> = expr.split_ascii_whitespace().collect();

        let conn = if expr.len() == 1 {
            WireConnection::Exact(WireTerm::new(expr[0]))
        } else if expr[0] == "NOT" {
            WireConnection::Not(WireTerm::new(expr[1]))
        } else {
            let op = match expr[1] {
                "AND" => BinOp::And,
                "OR" => BinOp::Or,
                "LSHIFT" => BinOp::LShift,
                "RSHIFT" => BinOp::RShift,
                _ => unreachable!(),
            };

            WireConnection::BinOp(op, WireTerm::new(expr[0]), WireTerm::new(expr[2]))
        };

        wires.insert(dst.to_owned(), conn);
    }

    wires
}

fn resolve(
    term: &WireTerm,
    wires: &HashMap<String, WireConnection>,
    cache: &mut HashMap<String, u16>,
) -> u16 {
    match term {
        WireTerm::Constant(x) => *x,
        WireTerm::Wire(wire_name) => {
            if let Some(x) = cache.get(wire_name) {
                *x
            } else {
                let x = match &wires[wire_name] {
                    WireConnection::Exact(a) => resolve(a, wires, cache),
                    WireConnection::BinOp(op, a, b) => {
                        let a = resolve(a, wires, cache);
                        let b = resolve(b, wires, cache);
                        match op {
                            BinOp::And => a & b,
                            BinOp::Or => a | b,
                            BinOp::LShift => a << b,
                            BinOp::RShift => a >> b,
                        }
                    }
                    WireConnection::Not(a) => !resolve(a, wires, cache),
                };
                cache.insert(wire_name.to_owned(), x);

                x
            }
        }
    }
}

fn star1(input: &HashMap<String, WireConnection>) -> u16 {
    let mut cache = HashMap::new();
    resolve(&WireTerm::Wire("a".to_owned()), input, &mut cache)
}

fn star2(input: &HashMap<String, WireConnection>) -> u16 {
    let original_a = resolve(&WireTerm::Wire("a".to_owned()), input, &mut HashMap::new());

    let mut updated_wires = input.clone();
    updated_wires.insert(
        "b".to_owned(),
        WireConnection::Exact(WireTerm::Constant(original_a)),
    );

    resolve(
        &WireTerm::Wire("a".to_owned()),
        &updated_wires,
        &mut HashMap::new(),
    )
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
