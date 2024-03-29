use regex::Regex;
use utils::read_input_file;

fn star1(input: &str) -> i32 {
    input
        .split_ascii_whitespace()
        .map(|line| {
            let mut iter = line.as_bytes().iter().filter(|c| c.is_ascii_digit());
            let first = iter.next().unwrap();
            let last = iter.last().unwrap_or(first);

            ((first - b'0') * 10 + (last - b'0')) as i32
        })
        .sum::<i32>()
}

fn str_to_digit(s: &str) -> i32 {
    match s {
        "one" => 1,
        "two" => 2,
        "three" => 3,
        "four" => 4,
        "five" => 5,
        "six" => 6,
        "seven" => 7,
        "eight" => 8,
        "nine" => 9,
        d => d.parse().unwrap(),
    }
}

fn star2(input: &str) -> i32 {
    let digit_regex = Regex::new(r"[0-9]|one|two|three|four|five|six|seven|eight|nine").unwrap();

    let extract_value = |line: &str| {
        // Huh, it looks like digits can overlap, so we have to do this weird thing
        let mut start = 0;
        let mut first = None;
        let mut last = None;
        while let Some(m) = digit_regex.find_at(line, start) {
            start = m.start() + 1;

            if first.is_none() {
                first = Some(str_to_digit(m.as_str()));
            }
            last = Some(str_to_digit(m.as_str()))
        }

        first.unwrap() * 10 + last.unwrap()
    };

    input
        .split_ascii_whitespace()
        .map(extract_value)
        .sum::<i32>()
}

fn main() {
    let input = read_input_file!();

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
