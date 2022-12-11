#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

#[derive(Debug, Clone, Copy)]
enum Operation {
    Mul(u64),
    Add(u64),
    Sq,
}

#[derive(Debug, Clone)]
struct Monkey {
    items: Vec<u64>,
    operation: Operation,
    test: u64,
    targets: [usize; 2],
}

#[derive(Debug, Clone)]
struct Monkeys {
    monkeys: Vec<Monkey>,
    inspect_count: Vec<usize>,
    lcm: u64,
}

impl Monkeys {
    fn new(monkeys: Vec<Monkey>) -> Self {
        // It looks like tests are all primes, so it's fine to just multiply them together
        let lcm = monkeys.iter().map(|m| m.test).product();

        Self {
            inspect_count: vec![0; monkeys.len()],
            monkeys,
            lcm,
        }
    }

    fn step_round(&mut self, limit_worries: bool) {
        for i in 0..self.monkeys.len() {
            // Inspecting the input shows that monkeys do not throw items back
            // to themselves, which would be messed up
            let items = {
                let mut tmp = Vec::new();
                std::mem::swap(&mut tmp, &mut self.monkeys[i].items);
                tmp
            };
            let Monkey {
                operation,
                test,
                targets,
                ..
            } = self.monkeys[i];
            self.inspect_count[i] += items.len();

            for mut item in items {
                match operation {
                    Operation::Add(x) => item += x,
                    Operation::Mul(x) => item *= x,
                    Operation::Sq => item *= item,
                }

                if limit_worries {
                    item /= 3;
                }

                item %= self.lcm;

                let test_result = item % test == 0;
                let target = targets[test_result as usize];

                self.monkeys[target].items.push(item);
            }
        }
    }

    fn monkey_business(&self) -> usize {
        let mut sorted = self.inspect_count.clone();
        sorted.sort_by(|i, j| j.cmp(i));
        sorted[0] * sorted[1]
    }
}

fn parse_input(input: String) -> Monkeys {
    let lines: Vec<_> = input.lines().collect();
    let monkeys = lines
        .chunks(7)
        .map(|chunk| {
            if let &[_, starting_items, operation, test, true_target, false_target, ..] = chunk {
                let starting_items = starting_items["  Starting items: ".len()..]
                    .split(", ")
                    .map(|i| i.parse().unwrap())
                    .collect();

                let operation = &operation["  Operation: new = old ".len()..];
                let operation = match operation.as_bytes()[0] {
                    b'+' => Operation::Add(operation[2..].parse().unwrap()),
                    b'*' => {
                        let coeff = &operation[2..];
                        if coeff == "old" {
                            Operation::Sq
                        } else {
                            Operation::Mul(coeff.parse().unwrap())
                        }
                    }
                    _ => unreachable!(),
                };

                let test = test["  Test: divisible by ".len()..].parse().unwrap();

                let true_target = true_target["    If true: throw to monkey ".len()..]
                    .parse()
                    .unwrap();
                let false_target = false_target["    If false: throw to monkey ".len()..]
                    .parse()
                    .unwrap();

                Monkey {
                    items: starting_items,
                    operation,
                    test,
                    targets: [false_target, true_target],
                }
            } else {
                unreachable!()
            }
        })
        .collect();

    Monkeys::new(monkeys)
}

fn star1(input: &Monkeys) -> usize {
    let mut monkeys = input.clone();

    for _ in 0..20 {
        monkeys.step_round(true);
    }

    monkeys.monkey_business()
}

fn star2(input: &Monkeys) -> usize {
    let mut monkeys = input.clone();

    for _ in 0..10000 {
        monkeys.step_round(false);
    }

    monkeys.monkey_business()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
