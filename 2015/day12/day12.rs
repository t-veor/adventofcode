#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! serde_json = "1.0"
//! ```

use serde_json::Value;

fn parse_input(input: String) -> Value {
    serde_json::from_str(&input).unwrap()
}

fn star1(input: &Value) -> i64 {
    fn visitor(v: &Value) -> i64 {
        match v {
            Value::Null | Value::Bool(_) | Value::String(_) => 0,
            Value::Number(n) => n.as_i64().unwrap(),
            Value::Array(values) => values.iter().map(visitor).sum(),
            Value::Object(map) => map.values().map(visitor).sum(),
        }
    }

    visitor(input)
}

fn star2(input: &Value) -> i64 {
    fn visitor(v: &Value) -> (i64, bool) {
        match v {
            Value::Null | Value::Bool(_) => (0, false),
            Value::String(s) => (0, s == "red"),
            Value::Number(n) => (n.as_i64().unwrap(), false),
            Value::Array(values) => (values.iter().map(|x| visitor(x).0).sum(), false),
            Value::Object(map) => {
                let mut total = 0;
                for value in map.values() {
                    let (x, is_red) = visitor(value);
                    if is_red {
                        return (0, false);
                    }
                    total += x;
                }
                (total, false)
            }
        }
    }

    visitor(input).0
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
