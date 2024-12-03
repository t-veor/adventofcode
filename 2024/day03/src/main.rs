use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, char, u32},
    combinator::value,
    error::ParseError,
    multi::fold_many0,
    sequence::{delimited, preceded, separated_pair},
    IResult, Parser,
};
use utils::read_input_file;

#[derive(Debug, Clone, Copy)]
enum Instr {
    Do,
    Dont,
    Mul(u32, u32),
}

fn mul_instr(input: &str) -> IResult<&str, (u32, u32)> {
    preceded(
        tag("mul"),
        delimited(char('('), separated_pair(u32, char(','), u32), char(')')),
    )(input)
}

fn do_instr(input: &str) -> IResult<&str, &str> {
    tag("do()")(input)
}

fn dont_instr(input: &str) -> IResult<&str, &str> {
    tag("don't()")(input)
}

fn parse_instr(input: &str) -> IResult<&str, Instr> {
    alt((
        mul_instr.map(|(a, b)| Instr::Mul(a, b)),
        value(Instr::Do, do_instr),
        value(Instr::Dont, dont_instr),
    ))(input)
}

fn read_all<'a, T, E: ParseError<&'a str>>(
    input: &'a str,
    parser: impl Parser<&'a str, T, E>,
) -> IResult<&'a str, Vec<T>, E> {
    fold_many0(
        alt((parser.map(Some), anychar.map(|_| None))),
        || Vec::new(),
        |mut acc, x| match x {
            Some(x) => {
                acc.push(x);
                acc
            }
            None => acc,
        },
    )(input)
}

fn star1(input: &str) -> u32 {
    let Ok(("", mul_instrs)) = read_all(input, mul_instr) else {
        panic!()
    };

    mul_instrs.into_iter().map(|(a, b)| a * b).sum()
}

fn star2(input: &str) -> u32 {
    let Ok(("", instrs)) = read_all(input, parse_instr) else {
        panic!()
    };

    let mut total = 0;
    let mut mul_enabled = true;

    for instr in instrs {
        match instr {
            Instr::Do => mul_enabled = true,
            Instr::Dont => mul_enabled = false,
            Instr::Mul(a, b) => {
                if mul_enabled {
                    total += a * b
                }
            }
        }
    }

    total
}

fn main() {
    let input = read_input_file!();

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
