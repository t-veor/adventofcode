use utils::read_input_file;

fn backtracking_search(target_design: &str, towels: &[&str]) -> bool {
    if target_design == "" {
        return true;
    }

    for towel in towels {
        if target_design.starts_with(towel)
            && backtracking_search(&target_design[towel.len()..], towels)
        {
            return true;
        }
    }

    false
}

fn dp_search(target_design: &str, towels: &[&str]) -> i64 {
    let mut memo = vec![0; target_design.len() + 1];
    memo[0] = 1;

    for i in 0..target_design.len() {
        if memo[i] == 0 {
            continue;
        }

        for towel in towels {
            if target_design[i..].starts_with(towel) {
                memo[i + towel.len()] += memo[i];
            }
        }
    }

    memo.last().copied().unwrap()
}

fn parse_input(input: &str) -> (Vec<&str>, Vec<&str>) {
    let (towels, designs) = input.split_once("\n\n").unwrap();

    let towels = towels.split(", ").collect();
    let designs = designs.lines().collect();

    (towels, designs)
}

fn star1(towels: &[&str], designs: &[&str]) -> usize {
    designs
        .iter()
        .filter(|design| backtracking_search(design, towels))
        .count()
}

fn star2(towels: &[&str], designs: &[&str]) -> i64 {
    designs.iter().map(|design| dp_search(design, towels)).sum()
}

fn main() {
    let input = read_input_file!();
    let (towels, designs) = parse_input(&input);

    println!("{}", star1(&towels, &designs));
    println!("{}", star2(&towels, &designs));
}
