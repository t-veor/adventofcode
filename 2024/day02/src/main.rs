use utils::read_input_file;

fn parse_input(input: &str) -> Vec<Vec<i32>> {
    input
        .trim()
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|x| x.parse().unwrap())
                .collect()
        })
        .collect()
}

fn report_is_safe(report: &[i32]) -> bool {
    let mut count_increasing = 0;
    let mut count_decreasing = 0;

    for window in report.windows(2) {
        let [a, b] = window else { unreachable!() };
        let diff = b - a;

        match diff {
            0 => return false,
            x if x.abs() > 3 => return false,
            x if x > 0 => count_increasing += 1,
            x if x < 0 => count_decreasing += 1,
            _ => unreachable!(),
        }
    }

    match (count_increasing, count_decreasing) {
        (0, _) | (_, 0) => true,
        _ => false,
    }
}

fn report_is_safe_dampened(report: &[i32]) -> bool {
    if report_is_safe(report) {
        return true;
    }

    for i in 0..report.len() {
        let mut cut_report = Vec::with_capacity(report.len() - 1);
        for j in 0..report.len() {
            if i == j {
                continue;
            }
            cut_report.push(report[j]);
        }

        if report_is_safe(&cut_report) {
            return true;
        }
    }

    false
}

fn star1(reports: &[Vec<i32>]) -> usize {
    reports
        .iter()
        .filter(|report| report_is_safe(report))
        .count()
}

fn star2(reports: &[Vec<i32>]) -> usize {
    reports
        .iter()
        .filter(|report| report_is_safe_dampened(report))
        .count()
}

fn main() {
    let input = read_input_file!();
    let reports = parse_input(&input);

    println!("{}", star1(&reports));
    println!("{}", star2(&reports));
}
