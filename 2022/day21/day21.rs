#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! gcd = "2.2"
//! ```

use gcd::Gcd;
use std::{
    collections::HashMap,
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Sub},
};

#[derive(Debug, Copy, Clone)]
enum Operation {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
enum Monkey {
    Value(i64),
    Op(Operation, String, String),
}

#[derive(Debug, Clone)]
struct Monkeys(HashMap<String, Monkey>);

fn parse_input(input: String) -> Monkeys {
    let mut monkeys = HashMap::new();

    for line in input.lines() {
        let (name, job) = line.split_once(": ").unwrap();
        let job: Vec<_> = job.split_ascii_whitespace().collect();
        let monkey = if job.len() == 1 {
            Monkey::Value(job[0].parse().unwrap())
        } else {
            let op = match job[1] {
                "+" => Operation::Add,
                "-" => Operation::Sub,
                "*" => Operation::Mul,
                "/" => Operation::Div,
                _ => unreachable!(),
            };
            Monkey::Op(op, job[0].to_owned(), job[2].to_owned())
        };

        monkeys.insert(name.to_owned(), monkey);
    }

    Monkeys(monkeys)
}

impl Monkeys {
    fn resolve(&self) -> i64 {
        fn recurse<'a>(
            monkey: &'a str,
            monkeys: &'a Monkeys,
            cache: &mut HashMap<&'a str, i64>,
        ) -> i64 {
            if let Some(x) = cache.get(monkey) {
                return *x;
            }

            let result = match &monkeys.0[monkey] {
                Monkey::Value(x) => *x,
                Monkey::Op(op, left, right) => {
                    let left = recurse(left, monkeys, cache);
                    let right = recurse(right, monkeys, cache);
                    match op {
                        Operation::Add => left + right,
                        Operation::Sub => left - right,
                        Operation::Mul => left * right,
                        Operation::Div => left / right,
                    }
                }
            };

            cache.insert(monkey, result);
            result
        }

        recurse("root", self, &mut HashMap::new())
    }
}

fn star1(input: &Monkeys) -> i64 {
    input.resolve()
}

// We could bring in a symbolic maths library, but what fun is that
#[derive(Debug, Clone, PartialEq, Eq)]
struct Polynomial(Vec<i64>);

impl Polynomial {
    fn new(mut coeffs: Vec<i64>) -> Self {
        let mut last_non_zero_coeff = 0;
        for (i, &coeff) in coeffs.iter().enumerate() {
            if coeff != 0 {
                last_non_zero_coeff = i + 1;
            }
        }
        coeffs.truncate(last_non_zero_coeff);
        Self(coeffs)
    }

    fn one() -> Self {
        Self(vec![1])
    }

    fn from_const(x: i64) -> Self {
        Self(vec![x])
    }

    fn x() -> Self {
        Self(vec![0, 1])
    }

    fn largest_coeff_negative(&self) -> bool {
        self.0.last().map(|&x| x < 1).unwrap_or(false)
    }

    fn gcd(&self) -> u64 {
        self.0.iter().copied().fold(0, |x, y| x.gcd(y.abs() as u64))
    }

    fn constant(&self) -> i64 {
        self.0.first().copied().unwrap_or(0)
    }

    fn highest_degree_coeff(&self) -> i64 {
        self.0.last().copied().unwrap_or(0)
    }
}

impl Display for Polynomial {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut has_term = false;
        for (i, &coeff) in self.0.iter().enumerate().rev() {
            if coeff == 0 {
                continue;
            }

            if !has_term {
                coeff.fmt(f)?;
            } else {
                if coeff > 0 {
                    f.write_str(" + ")?;
                } else {
                    f.write_str(" - ")?;
                }
                coeff.abs().fmt(f)?;
            }

            if i > 0 {
                f.write_str("x")?;
                if i > 1 {
                    f.write_fmt(format_args!("^{i}"))?;
                }
            }

            has_term = true;
        }

        if !has_term {
            f.write_str("0")?;
        }

        Ok(())
    }
}

impl Add for Polynomial {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let max_len = self.0.len().max(rhs.0.len());
        let mut coeffs = Vec::with_capacity(max_len);
        for i in 0..max_len {
            let coeff = self.0.get(i).copied().unwrap_or(0) + rhs.0.get(i).copied().unwrap_or(0);
            coeffs.push(coeff);
        }
        Self::new(coeffs)
    }
}

impl Neg for Polynomial {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        for x in self.0.iter_mut() {
            *x = -*x;
        }
        self
    }
}

impl Sub for Polynomial {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl Mul for Polynomial {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let max_len = (self.0.len() + rhs.0.len()).saturating_sub(1);
        let mut coeffs = vec![0; max_len];
        for (i, x) in self.0.iter().copied().enumerate() {
            for (j, y) in rhs.0.iter().copied().enumerate() {
                coeffs[i + j] += x * y;
            }
        }
        Self::new(coeffs)
    }
}

impl Div<i64> for Polynomial {
    type Output = Self;

    fn div(mut self, rhs: i64) -> Self::Output {
        self.0.iter_mut().for_each(|x| *x /= rhs);
        self
    }
}

#[derive(Debug, Clone)]
struct RationalFunction {
    numerator: Polynomial,
    denominator: Polynomial,
}

impl RationalFunction {
    fn new(mut numerator: Polynomial, mut denominator: Polynomial) -> Self {
        if denominator.largest_coeff_negative() {
            numerator = -numerator;
            denominator = -denominator;
        }

        let gcd = numerator.gcd().gcd(denominator.gcd());
        if gcd > 1 {
            numerator = numerator / gcd as i64;
            denominator = denominator / gcd as i64;
        }

        Self {
            numerator,
            denominator,
        }
    }

    fn from_const(x: i64) -> Self {
        Self {
            numerator: Polynomial(vec![x]),
            denominator: Polynomial::one(),
        }
    }

    fn x() -> Self {
        Self {
            numerator: Polynomial::x(),
            denominator: Polynomial::one(),
        }
    }
}

impl Display for RationalFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.denominator == Polynomial::one() {
            f.write_fmt(format_args!("{}", self.numerator))
        } else {
            f.write_fmt(format_args!(
                "({}) / ({})",
                self.numerator, self.denominator
            ))
        }
    }
}

impl Add for RationalFunction {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.denominator == rhs.denominator {
            Self::new(self.numerator + rhs.numerator, self.denominator)
        } else {
            // we could try to do a least common multiple thing but I'm lazy
            Self::new(
                self.numerator * rhs.denominator.clone() + rhs.numerator * self.denominator.clone(),
                self.denominator * rhs.denominator,
            )
        }
    }
}

impl Neg for RationalFunction {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::new(-self.numerator, self.denominator)
    }
}

impl Sub for RationalFunction {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl Mul for RationalFunction {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        // Could do some simplification, but again lazy
        Self::new(
            self.numerator * rhs.numerator,
            self.denominator * rhs.denominator,
        )
    }
}

impl Div for RationalFunction {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::new(
            self.numerator * rhs.denominator,
            self.denominator * rhs.numerator,
        )
    }
}

impl Monkeys {
    fn resolve2(&self) -> (RationalFunction, RationalFunction) {
        fn recurse<'a>(
            monkey: &'a str,
            monkeys: &'a Monkeys,
            cache: &mut HashMap<&'a str, RationalFunction>,
        ) -> RationalFunction {
            if monkey == "humn" {
                return RationalFunction::x();
            }

            if let Some(x) = cache.get(monkey) {
                return x.clone();
            }

            let result = match &monkeys.0[monkey] {
                Monkey::Value(x) => RationalFunction::from_const(*x),
                Monkey::Op(op, left, right) => {
                    let left = recurse(left, monkeys, cache);
                    let right = recurse(right, monkeys, cache);
                    match op {
                        Operation::Add => left + right,
                        Operation::Sub => left - right,
                        Operation::Mul => left * right,
                        Operation::Div => left / right,
                    }
                }
            };

            cache.insert(monkey, result.clone());
            result
        }

        let mut cache = HashMap::new();
        match &self.0["root"] {
            Monkey::Value(_) => unreachable!(),
            Monkey::Op(_, left, right) => {
                let left = recurse(left, self, &mut cache);
                let right = recurse(right, self, &mut cache);
                (left, right)
            }
        }
    }
}

fn star2(input: &Monkeys) -> i64 {
    let (left, right) = input.resolve2();
    // Ahh man, I thought there was going to be something a bit more interesting
    // or I wouldn't have implemented this entire rational function machinery.
    // It looks like that you just end up with a linear equation.

    // cross-multiply
    let (mut left, mut right) = (
        left.numerator * right.denominator,
        right.numerator * left.denominator,
    );
    // remove the constant from the left side
    let constant = Polynomial::from_const(left.constant());
    left = left - constant.clone();
    right = right - constant;

    // Divide by the coefficient on the x
    let coeff = left.highest_degree_coeff();
    // left = left / coeff;
    right = right / coeff;

    right.constant()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
