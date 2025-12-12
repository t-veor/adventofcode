use utils::read_input_file;

#[derive(Debug)]
struct Problem {
    size: (u32, u32),
    counts: Vec<u32>,
}

fn parse_input(input: &str) -> (Vec<u32>, Vec<Problem>) {
    let sections: Vec<_> = input.split("\n\n").collect();
    let [tiles @ .., problems] = sections.as_slice() else {
        panic!()
    };

    // It turns out nothing about the tiles matter except for how many spaces they occupy.
    let tiles = tiles
        .iter()
        .map(|tile| tile.chars().filter(|&c| c == '#').count() as u32)
        .collect();

    let problems = problems
        .lines()
        .map(|line| {
            let (size, counts) = line.split_once(": ").unwrap();
            let (width, height) = size.split_once("x").unwrap();
            let counts = counts.split(" ").map(|i| i.parse().unwrap()).collect();

            Problem {
                size: (width.parse().unwrap(), height.parse().unwrap()),
                counts,
            }
        })
        .collect();

    (tiles, problems)
}

fn star1(tiles: &[u32], problems: &[Problem]) -> u32 {
    let mut definitely_possible = 0;
    let mut indeterminate = 0;

    for problem in problems.iter() {
        let (width, height) = problem.size;
        // If we can the required number of 3*3 squares into the region, then it's definitely
        // possible by just putting each present into that region
        let simple_fit_bound = problem.counts.iter().sum::<u32>() * 9;

        if (width / 3 * 3) * (height / 3 * 3) >= simple_fit_bound {
            definitely_possible += 1;
            continue;
        }

        // If there are more occupied spaces than the number of cells available in the region, it's
        // definitely impossible
        let minimum_required_cells = problem.counts.iter().zip(tiles).map(|(a, b)| a * b).sum();
        if width * height < minimum_required_cells {
            // definitely impossible
            continue;
        }

        // Otherwise you'd actually have to do it to try to figure out if it's possible or not.
        // This shouldn't happen in the actual input.
        indeterminate += 1;
    }

    if indeterminate > 0 {
        println!(
            "{indeterminate} regions could not be determined by a simple bound, and the answer is likely wrong."
        )
    }

    definitely_possible
}

fn main() {
    let input = read_input_file!();
    let (tiles, problems) = parse_input(&input);

    println!("{}", star1(&tiles, &problems));
}
