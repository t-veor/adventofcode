use utils::{parse::split_parse, read_input_file};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operator {
    Add,
    Mul,
}

fn split_numbers_and_operators(input: &str) -> (Vec<&str>, Vec<Operator>) {
    let lines: Vec<_> = input.lines().collect();
    let [numbers @ .., operators] = &lines[..] else {
        panic!("Not enough lines in input")
    };

    let operators: Vec<Operator> = operators
        .split_whitespace()
        .map(|c| match c {
            "+" => Operator::Add,
            "*" => Operator::Mul,
            _ => panic!("Unknown operator {c}"),
        })
        .collect();

    (numbers.to_vec(), operators)
}

fn transpose<T: Clone>(data: &[Vec<T>]) -> Vec<Vec<T>> {
    if data.is_empty() {
        return Vec::new();
    }

    (0..data[0].len())
        .map(|i| data.iter().map(|row| row[i].clone()).collect())
        .collect()
}

fn solve(numbers: &[Vec<i64>], operators: &[Operator]) -> i64 {
    numbers
        .iter()
        .enumerate()
        .map(|(i, line)| match operators[i] {
            Operator::Add => line.iter().sum::<i64>(),
            Operator::Mul => line.iter().product::<i64>(),
        })
        .sum()
}

fn star1(input: &str) -> i64 {
    let (numbers, operators) = split_numbers_and_operators(input);
    let numbers = transpose(&numbers.iter().map(|s| split_parse(s)).collect::<Vec<_>>());

    solve(&numbers, &operators)
}

fn star2(input: &str) -> i64 {
    let (numbers, operators) = split_numbers_and_operators(input);

    let numbers = transpose(
        &numbers
            .iter()
            .map(|line| line.chars().collect())
            .collect::<Vec<_>>(),
    );
    let numbers: Vec<Vec<_>> = numbers
        .split(|line| line.iter().all(|c| c.is_whitespace()))
        .map(|ns| {
            ns.iter()
                .map(|n| n.iter().collect::<String>().trim().parse::<i64>().unwrap())
                .collect()
        })
        .collect();

    solve(&numbers, &operators)
}

fn main() {
    let input = read_input_file!();

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
