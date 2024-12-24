use std::collections::{HashMap, HashSet};

use utils::read_input_file;

// Partial automated solution. For part 2, I got the program to print out bits
// which summed incorrectly, then manually went over the graphs following those
// bits and managed to identify 4 pairs of wires that were swapped.

#[derive(Debug, Clone, Copy)]
enum GateType {
    And,
    Or,
    Xor,
}

#[derive(Clone)]
struct WireGraph {
    wires: HashSet<String>,
    mappings: HashMap<String, bool>,
    gates: HashMap<String, (GateType, String, String)>,
}

impl WireGraph {
    fn exec(&mut self, target: &str) -> bool {
        if let Some(&result) = self.mappings.get(target) {
            return result;
        }

        let (gate, left, right) = self.gates[target].clone();

        let left_result = self.exec(&left);
        let right_result = self.exec(&right);

        let result = match gate {
            GateType::And => left_result & right_result,
            GateType::Or => left_result | right_result,
            GateType::Xor => left_result ^ right_result,
        };

        self.mappings.insert(target.to_string(), result);
        result
    }

    fn get_by_prefix(&self, prefix: char) -> Vec<String> {
        let mut wires: Vec<_> = self
            .wires
            .iter()
            .filter(|i| i.starts_with(prefix))
            .cloned()
            .collect();
        wires.sort();
        wires
    }

    fn read_zs(&mut self) -> u64 {
        self.get_by_prefix('z')
            .into_iter()
            .rev()
            .map(|i| self.exec(&i))
            .fold(0, |acc, b| acc * 2 + u64::from(b))
    }

    fn reset_mappings(&mut self) {
        self.mappings.clear();
    }

    fn write_x(&mut self, mut x: u64) {
        for i in self.get_by_prefix('x') {
            self.mappings.insert(i, x % 2 == 1);
            x /= 2;
        }
    }

    fn write_y(&mut self, mut y: u64) {
        for i in self.get_by_prefix('y') {
            self.mappings.insert(i, y % 2 == 1);
            y /= 2;
        }
    }

    #[allow(unused)]
    fn print_gates_canonical(&self) {
        let mut output = Vec::new();
        for (target, (gate, left, right)) in self.gates.iter() {
            let (a, b) = if left > right {
                (right, left)
            } else {
                (left, right)
            };

            let gate = match gate {
                GateType::And => "AND",
                GateType::Or => "OR",
                GateType::Xor => "XOR",
            };

            output.push(format!("{a} {gate} {b} -> {target}"));
        }

        output.sort();
        println!("{}", output.join("\n"));
    }
}

fn parse_input(input: &str) -> WireGraph {
    let mut wires = HashSet::new();
    let (mappings, gates) = input.split_once("\n\n").unwrap();

    let mappings = mappings
        .lines()
        .map(|line| {
            let (name, assignment) = line.split_once(": ").unwrap();
            wires.insert(name.to_string());
            (name.to_string(), assignment == "1")
        })
        .collect();

    let gates = gates
        .lines()
        .map(|line| {
            let (body, target) = line.split_once(" -> ").unwrap();
            let [left, gate, right] = body.split_whitespace().collect::<Vec<_>>()[..] else {
                unreachable!()
            };

            let gate = match gate {
                "AND" => GateType::And,
                "OR" => GateType::Or,
                "XOR" => GateType::Xor,
                _ => unreachable!(),
            };

            wires.insert(target.to_string());
            wires.insert(left.to_string());
            wires.insert(right.to_string());

            (
                target.to_string(),
                (gate, left.to_string(), right.to_string()),
            )
        })
        .collect();

    WireGraph {
        wires,
        mappings,
        gates,
    }
}

fn star1(wires: &WireGraph) -> u64 {
    wires.clone().read_zs()
}

fn print_wrong_bits(wires: &WireGraph) {
    let mut wires = wires.clone();

    for i in 0..45 {
        let x = 1 << i;
        wires.reset_mappings();
        wires.write_x(x);
        wires.write_y(x);

        let result = wires.read_zs();

        if result != x * 2 {
            println!("{i}");
        }
    }
}

fn main() {
    let input = read_input_file!();
    let wires = parse_input(&input);

    println!("{}", star1(&wires));
    println!("These bits sum to incorrect values:");
    print_wrong_bits(&wires);
}
