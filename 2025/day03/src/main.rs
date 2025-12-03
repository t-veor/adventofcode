use utils::read_input_file;

fn parse_input(input: &str) -> Vec<Vec<u8>> {
    input
        .split_whitespace()
        .map(|line| line.bytes().map(|c| c - b'0').collect())
        .collect()
}

// Might move this into the utils library? It's a little specific to be a general util
fn argmax_first(xs: &[u8]) -> Option<(usize, u8)> {
    let max = *xs.iter().max()?;
    let max_pos = xs.iter().position(|&x| x == max).unwrap();

    Some((max_pos, max))
}

fn max_joltage(bank: &[u8], batteries: usize) -> u64 {
    debug_assert!(bank.len() >= batteries);

    if batteries == 0 {
        0
    } else {
        let remaining_batteries = batteries - 1;

        // Since the size of any integer is dominated by its leading digit, the largest number we
        // can make is always going to be by greedily picking the biggest lead digit we can.
        // We have to make sure however that there are enough remaining digits for the rest of the
        // batteries, so we only pick a digit out of the first
        // 0..(bank.len() - remaining_batteries) digits.

        let (max_digit_pos, max_digit) =
            argmax_first(&bank[..bank.len() - remaining_batteries]).unwrap();

        10u64.pow(remaining_batteries as _) * max_digit as u64
            + max_joltage(&bank[max_digit_pos + 1..], remaining_batteries)
    }
}

fn star1(banks: &[Vec<u8>]) -> u64 {
    banks.iter().map(|bank| max_joltage(bank, 2)).sum()
}

fn star2(banks: &[Vec<u8>]) -> u64 {
    banks.iter().map(|bank| max_joltage(bank, 12)).sum()
}

fn main() {
    let input = read_input_file!();
    let banks = parse_input(&input);

    println!("{}", star1(&banks));
    println!("{}", star2(&banks));
}
