use std::ops::ControlFlow;

use utils::read_input_file;

#[derive(Debug, Clone, Copy)]
enum Operator {
    Add,
    Mul,
    Concat,
}

fn eval_expression(nums: &[i64], ops: &[Operator]) -> i64 {
    assert_eq!(nums.len(), ops.len() + 1);

    let mut acc = nums[0];
    for (x, op) in nums[1..].iter().zip(ops) {
        match op {
            Operator::Add => acc += x,
            Operator::Mul => acc *= x,
            Operator::Concat => {
                acc *= (10i64).pow(x.ilog10() + 1);
                acc += x;
            }
        }
    }

    acc
}

fn foreach_op_combo(
    required_len: usize,
    alphabet: &[Operator],
    mut cb: impl FnMut(&[Operator]) -> ControlFlow<()>,
) -> ControlFlow<()> {
    fn recurse(
        remaining_len: usize,
        alphabet: &[Operator],
        ops: &mut Vec<Operator>,
        cb: &mut impl FnMut(&[Operator]) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        if remaining_len == 0 {
            return cb(ops);
        }

        for &op in alphabet {
            ops.push(op);
            recurse(remaining_len - 1, alphabet, ops, cb)?;
            ops.pop();
        }

        ControlFlow::Continue(())
    }

    recurse(required_len, alphabet, &mut Vec::new(), &mut cb)
}

fn check_equation(target: i64, nums: &[i64], alphabet: &[Operator]) -> bool {
    assert!(!nums.is_empty());

    foreach_op_combo(nums.len() - 1, alphabet, |ops| {
        if target == eval_expression(nums, ops) {
            ControlFlow::Break(())
        } else {
            ControlFlow::Continue(())
        }
    })
    .is_break()
}

fn parse_input(input: &str) -> Vec<(i64, Vec<i64>)> {
    input
        .lines()
        .map(|line| {
            let (target, nums) = line.split_once(": ").unwrap();
            (
                target.parse().unwrap(),
                nums.split_whitespace()
                    .map(|n| n.parse().unwrap())
                    .collect(),
            )
        })
        .collect()
}

fn star1(equations: &[(i64, Vec<i64>)]) -> i64 {
    let mut total = 0;
    let alphabet = &[Operator::Add, Operator::Mul];

    for (target, nums) in equations {
        if check_equation(*target, nums, alphabet) {
            total += target;
        }
    }

    total
}

fn star2(equations: &[(i64, Vec<i64>)]) -> i64 {
    let mut total = 0;
    let alphabet = &[Operator::Add, Operator::Mul, Operator::Concat];

    for (target, nums) in equations {
        if check_equation(*target, nums, alphabet) {
            total += target;
        }
    }

    total
}

fn main() {
    let input = read_input_file!();
    let equations = parse_input(&input);

    println!("{}", star1(&equations));
    println!("{}", star2(&equations));
}
