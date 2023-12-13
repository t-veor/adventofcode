use utils::read_input_file;

#[derive(Debug, Clone, Copy)]
enum Reflection {
    Horizontal(usize),
    Vertical(usize),
}

fn parse_input(input: &str) -> Vec<Vec<Vec<u8>>> {
    input
        .split("\n\n")
        .map(|chunk| chunk.lines().map(|line| line.as_bytes().to_vec()).collect())
        .collect()
}

fn transpose(grid: &[Vec<u8>]) -> Vec<Vec<u8>> {
    if grid.is_empty() {
        return Vec::new();
    }

    let mut transposed: Vec<_> = (0..grid[0].len())
        .map(|_| Vec::with_capacity(grid.len()))
        .collect();

    for row in grid {
        for (i, &element) in row.iter().enumerate() {
            transposed[i].push(element);
        }
    }

    transposed
}

fn hamming_distance(xs: &[u8], ys: &[u8]) -> usize {
    xs.iter().zip(ys.iter()).filter(|(x, y)| x != y).count()
}

fn find_horizontal_reflection(
    grid: &[Vec<u8>],
    allowed_smudges: usize,
) -> Option<usize> {
    if grid.len() <= 1 {
        // If there is only 1 row it doesn't make sense to consider it reflected
        return None;
    }

    (1..grid.len()).find(|&i| {
        let mut smudges = 0;

        let start = (2 * i).saturating_sub(grid.len());
        for j in start..i {
            let k = 2 * i - j - 1;

            smudges += hamming_distance(&grid[j], &grid[k]);

            if smudges > allowed_smudges {
                return false;
            }
        }

        smudges == allowed_smudges
    })
}

fn find_reflection(grid: &[Vec<u8>], allowed_smudges: usize) -> Option<Reflection> {
    if let Some(rows) = find_horizontal_reflection(&grid, allowed_smudges) {
        return Some(Reflection::Horizontal(rows));
    }

    let transposed = transpose(grid);
    if let Some(columns) = find_horizontal_reflection(&transposed, allowed_smudges) {
        return Some(Reflection::Vertical(columns));
    }

    None
}

fn summarize_reflections(input: &[Vec<Vec<u8>>], allowed_smudges: usize) -> usize {
    let mut total_rows = 0;
    let mut total_columns = 0;

    for (i, grid) in input.iter().enumerate() {
        match find_reflection(grid, allowed_smudges) {
            Some(Reflection::Horizontal(rows)) => total_rows += rows,
            Some(Reflection::Vertical(columns)) => total_columns += columns,
            None => panic!("Could not find reflection on grid {i}"),
        }
    }

    total_rows * 100 + total_columns
}

fn star1(input: &[Vec<Vec<u8>>]) -> usize {
    summarize_reflections(input, 0)
}

fn star2(input: &[Vec<Vec<u8>>]) -> usize {
    summarize_reflections(input, 1)
}

fn main() {
    let input = read_input_file!();
    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
