use utils::read_input_file;

#[derive(Debug)]
struct EmptyRowColData {
    // i-th element is no. of empty rows before i-th row
    prefixed_empty_rows: Vec<usize>,
    // i-th element is no. of empty columns before i-th column
    prefixed_empty_cols: Vec<usize>,
}

impl EmptyRowColData {
    fn gather(input: &[Vec<u8>]) -> Self {
        let width = input.first().unwrap().len();
        let height = input.len();

        let mut prefixed_empty_rows = Vec::with_capacity(height);
        let mut prefixed_empty_cols = Vec::with_capacity(width);

        let mut empty_rows = 0;
        for row in input.iter() {
            prefixed_empty_rows.push(empty_rows);
            if row.iter().all(|&c| c == b'.') {
                empty_rows += 1;
            }
        }

        let mut empty_cols = 0;
        for x in 0..width {
            prefixed_empty_cols.push(empty_cols);
            if (0..height).all(|y| input[y][x] == b'.') {
                empty_cols += 1;
            }
        }

        Self {
            prefixed_empty_rows,
            prefixed_empty_cols,
        }
    }
}

fn parse_input(input: &str) -> Vec<Vec<u8>> {
    input.lines().map(|line| line.as_bytes().to_vec()).collect()
}

fn gather_star_coords(input: &[Vec<u8>]) -> Vec<(usize, usize)> {
    let mut coords = Vec::new();

    for (y, row) in input.iter().enumerate() {
        for (x, &c) in row.iter().enumerate() {
            if c == b'#' {
                coords.push((x, y));
            }
        }
    }

    coords
}

fn calculate_pairwise_lengths(input: &[Vec<u8>], expansion_factor: i64) -> i64 {
    let coords = gather_star_coords(input);
    let empty = EmptyRowColData::gather(input);

    let mut sum = 0;
    for i in 0..coords.len() {
        for j in i..coords.len() {
            let (a, b) = (coords[i], coords[j]);

            let max_x = a.0.max(b.0);
            let min_x = a.0.min(b.0);
            let max_y = a.1.max(b.1);
            let min_y = a.1.min(b.1);

            let empty_col_count =
                empty.prefixed_empty_cols[max_x] - empty.prefixed_empty_cols[min_x];
            let empty_row_count =
                empty.prefixed_empty_rows[max_y] - empty.prefixed_empty_rows[min_y];

            sum += (max_x - min_x) as i64
                + (max_y - min_y) as i64
                + (expansion_factor - 1) * (empty_col_count + empty_row_count) as i64;
        }
    }

    sum
}

fn star1(input: &[Vec<u8>]) -> i64 {
    calculate_pairwise_lengths(input, 2)
}

fn star2(input: &[Vec<u8>]) -> i64 {
    calculate_pairwise_lengths(input, 1_000_000)
}

fn main() {
    let input = read_input_file!();
    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
