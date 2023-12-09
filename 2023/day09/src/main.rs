use utils::read_input_file;

fn parse_input(input: &str) -> Vec<Vec<i32>> {
    input
        .lines()
        .map(|line| {
            line.split_ascii_whitespace()
                .map(|i| i.parse().unwrap())
                .collect()
        })
        .collect()
}

fn get_deltas(xs: &[i32]) -> Vec<i32> {
    xs.windows(2).map(|window| window[1] - window[0]).collect()
}

fn extrapolate(sequence: &[i32]) -> i32 {
    if sequence.iter().all(|&x| x == 0) {
        0
    } else {
        let deltas = get_deltas(sequence);
        let next_delta = extrapolate(&deltas);
        sequence.last().unwrap() + next_delta
    }
}

fn star1(sequences: &[Vec<i32>]) -> i32 {
    sequences.iter().map(|sequence| extrapolate(sequence)).sum()
}

fn star2(sequences: &[Vec<i32>]) -> i32 {
    sequences
        .iter()
        .map(|sequence| {
            let mut sequence = sequence.clone();
            sequence.reverse();
            extrapolate(&sequence)
        })
        .sum()
}

fn main() {
    let input = read_input_file!();
    let sequences = parse_input(&input);

    println!("{}", star1(&sequences));
    println!("{}", star2(&sequences));
}
