use std::collections::HashMap;

use utils::read_input_file;

#[derive(Debug, Clone)]
struct Part([i64; 4]);

#[derive(Debug, Clone, Copy)]
enum Operator {
    LessThan,
    GreaterThan,
}

#[derive(Debug, Clone)]
enum Instruction {
    Condition {
        target: String,
        index: usize,
        operator: Operator,
        value: i64,
    },
    SendTo {
        target: String,
    },
}

#[derive(Debug, Clone, Copy)]
enum WorkflowResult<'a> {
    Rejected,
    Accepted,
    SendTo { target: &'a str },
}

const RANGE_MIN: i64 = 1;
const RANGE_MAX: i64 = 4000;

fn parse_input(input: &str) -> (HashMap<String, Vec<Instruction>>, Vec<Part>) {
    let (workflows, parts) = input.split_once("\n\n").unwrap();

    let workflows: HashMap<_, _> = workflows
        .lines()
        .map(|workflow| {
            let (name, instructions) = workflow.split_once('{').unwrap();
            let instructions: Vec<_> = instructions
                .strip_suffix('}')
                .unwrap()
                .split(',')
                .map(|instruction| {
                    if let Some((condition, target)) = instruction.split_once(':') {
                        let op_idx = condition.find(&['<', '>']).unwrap();
                        let property = &condition[..op_idx];
                        let op = &condition[op_idx..op_idx + 1];
                        let value = &condition[op_idx + 1..];

                        let index = match property {
                            "x" => 0,
                            "m" => 1,
                            "a" => 2,
                            "s" => 3,
                            _ => panic!("Unknown property {property}"),
                        };

                        let operator = match op {
                            "<" => Operator::LessThan,
                            ">" => Operator::GreaterThan,
                            _ => unreachable!(),
                        };

                        let value = value.parse().unwrap();
                        Instruction::Condition {
                            target: target.to_string(),
                            index,
                            operator,
                            value,
                        }
                    } else {
                        Instruction::SendTo {
                            target: instruction.to_string(),
                        }
                    }
                })
                .collect();

            (name.to_string(), instructions)
        })
        .collect();

    let parts: Vec<_> = parts
        .lines()
        .map(|line| {
            let mut values = [0; 4];
            let components = line
                .strip_prefix('{')
                .unwrap()
                .strip_suffix('}')
                .unwrap()
                .split(',');

            for component in components {
                let (property, value) = component.split_once('=').unwrap();
                let index = match property {
                    "x" => 0,
                    "m" => 1,
                    "a" => 2,
                    "s" => 3,
                    _ => panic!("Unknown property {property}"),
                };
                values[index] = value.parse().unwrap();
            }

            Part(values)
        })
        .collect();

    (workflows, parts)
}

fn process_workflow<'a>(workflow: &'a [Instruction], part: &Part) -> WorkflowResult<'a> {
    let mut destination: Option<&str> = None;

    for instruction in workflow {
        match instruction {
            Instruction::Condition {
                target,
                index,
                operator,
                value,
            } => {
                let actual_value = part.0[*index];
                let matched = match operator {
                    Operator::LessThan => actual_value < *value,
                    Operator::GreaterThan => actual_value > *value,
                };

                if matched {
                    destination = Some(&target);
                    break;
                }
            }
            Instruction::SendTo { target } => {
                destination = Some(target);
                break;
            }
        }
    }

    match destination {
        Some("R") => WorkflowResult::Rejected,
        Some("A") => WorkflowResult::Accepted,
        Some(target) => WorkflowResult::SendTo { target },
        None => panic!("Workflow did not find a result"),
    }
}

fn check_part_accepted(workflows: &HashMap<String, Vec<Instruction>>, part: &Part) -> bool {
    let mut current_workflow = "in";
    loop {
        match process_workflow(&workflows[current_workflow], part) {
            WorkflowResult::Rejected => return false,
            WorkflowResult::Accepted => return true,
            WorkflowResult::SendTo { target } => current_workflow = target,
        }
    }
}

fn star1(workflows: &HashMap<String, Vec<Instruction>>, parts: &[Part]) -> i64 {
    let mut sum = 0;
    for part in parts {
        if check_part_accepted(workflows, part) {
            sum += part.0.iter().sum::<i64>();
        }
    }

    sum
}

#[derive(Debug, Clone)]
struct Intervals([(i64, i64); 4]);

impl Intervals {
    fn full() -> Self {
        Self([(RANGE_MIN, RANGE_MAX + 1); 4])
    }

    fn count(&self) -> i64 {
        self.0.iter().map(|&(a, b)| (b - a).max(0)).product()
    }
}

fn get_possible_accepts(
    workflows: &HashMap<String, Vec<Instruction>>,
    workflow: &str,
    mut possible_intervals: Intervals,
) -> i64 {
    let mut accepts = 0;

    for instruction in workflows[workflow].iter() {
        match instruction {
            &Instruction::Condition {
                ref target,
                index,
                operator,
                value,
            } => {
                let mut branch_intervals = possible_intervals.clone();
                match operator {
                    Operator::LessThan => {
                        branch_intervals.0[index].1 = branch_intervals.0[index].1.min(value);
                        possible_intervals.0[index].0 = possible_intervals.0[index].0.max(value);
                    }
                    Operator::GreaterThan => {
                        branch_intervals.0[index].0 = branch_intervals.0[index].0.max(value + 1);
                        possible_intervals.0[index].1 =
                            possible_intervals.0[index].1.min(value + 1);
                    }
                }

                match target.as_str() {
                    "R" => (),
                    "A" => accepts += branch_intervals.count(),
                    target => accepts += get_possible_accepts(workflows, target, branch_intervals),
                }
            }
            Instruction::SendTo { target } => {
                match target.as_str() {
                    "R" => (),
                    "A" => accepts += possible_intervals.count(),
                    target => {
                        accepts += get_possible_accepts(workflows, target, possible_intervals)
                    }
                }
                break;
            }
        }
    }
    accepts
}

fn star2(workflows: &HashMap<String, Vec<Instruction>>) -> i64 {
    get_possible_accepts(workflows, "in", Intervals::full())
}

fn main() {
    let input = read_input_file!();
    let (workflows, parts) = parse_input(&input);

    println!("{}", star1(&workflows, &parts));
    println!("{}", star2(&workflows));
}
