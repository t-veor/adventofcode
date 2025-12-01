use utils::read_input_file;

const DIAL_SIZE: i32 = 100;
const INITIAL_DIAL: i32 = 50;

fn parse_input(input: &str) -> Vec<i32> {
    input
        .trim()
        .lines()
        .map(|line| {
            if let Some(n) = line.strip_prefix('L') {
                -n.parse::<i32>().unwrap()
            } else if let Some(n) = line.strip_prefix('R') {
                n.parse().unwrap()
            } else {
                panic!("Unparseable line {line}")
            }
        })
        .collect()
}

fn star1(instructions: &[i32]) -> i32 {
    let mut dial = INITIAL_DIAL;
    let mut times_zeroed = 0;

    for &x in instructions {
        dial = (dial + x).rem_euclid(INITIAL_DIAL);
        if dial == 0 {
            times_zeroed += 1;
        }
    }

    times_zeroed
}

fn star2(instructions: &[i32]) -> i32 {
    let mut dial = INITIAL_DIAL;
    let mut times_zeroed = 0;

    for &x in instructions {
        if x > 0 {
            dial += x;

            times_zeroed += dial / DIAL_SIZE;
            dial %= DIAL_SIZE;
        } else if x < 0 {
            // There's a bunch of edge cases around negative division.
            // To avoid them we just transform the problem so that x is positive, perform the
            // calculation, and then transform back.
            dial = (-dial).rem_euclid(DIAL_SIZE);
            let x = -x;

            dial += x;

            times_zeroed += dial / DIAL_SIZE;
            dial %= DIAL_SIZE;

            dial = (-dial).rem_euclid(DIAL_SIZE);
        }
    }

    times_zeroed
}

fn main() {
    let input = read_input_file!();
    let instructions = parse_input(&input);

    println!("{}", star1(&instructions));
    println!("{}", star2(&instructions));
}
