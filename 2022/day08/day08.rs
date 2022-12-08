#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

struct TreeGrid {
    grid: Vec<i8>,
    width: usize,
}

fn parse_input(input: String) -> TreeGrid {
    let mut grid = Vec::new();
    let mut width = 0;
    for line in input.lines() {
        width = line.len();
        grid.extend(line.as_bytes().iter().map(|x| (*x - b'0') as i8));
    }

    TreeGrid { grid, width }
}

fn determine_visibility(grid: &TreeGrid) -> Vec<bool> {
    let mut visibility = vec![false; grid.grid.len()];
    let width = grid.width;
    let height = grid.grid.len() / grid.width;

    fn check_visibility_along_line(
        line_indices: impl Iterator<Item = usize>,
        grid: &TreeGrid,
        visibility: &mut [bool],
    ) {
        let mut max_height_so_far = -1i8;
        for i in line_indices {
            if grid.grid[i] > max_height_so_far {
                max_height_so_far = grid.grid[i];
                visibility[i] = true;
            }
        }
    }

    for col in 0..width {
        // north-south
        check_visibility_along_line((col..grid.grid.len()).step_by(width), grid, &mut visibility);
        // south-north
        check_visibility_along_line(
            (col..grid.grid.len()).step_by(width).rev(),
            grid,
            &mut visibility,
        );
    }

    for row in 0..height {
        // west-east
        check_visibility_along_line(row * width..(row + 1) * width, grid, &mut visibility);
        // south-north
        check_visibility_along_line(
            (row * width..(row + 1) * width).rev(),
            grid,
            &mut visibility,
        );
    }

    visibility
}

fn determine_scenic_score(grid: &TreeGrid) -> Vec<i32> {
    let mut scenic_score = vec![1; grid.grid.len()];
    let width = grid.width;
    let height = grid.grid.len() / grid.width;

    fn calculate_score_along_line(
        line_indices: impl Iterator<Item = usize>,
        grid: &TreeGrid,
        scenic_score: &mut [i32],
    ) {
        let mut closest_tree_pos_with_height_greater_or_equal_to = [0; 10];
        for (i, idx) in line_indices.enumerate() {
            let tree_height = grid.grid[idx] as usize;
            let closest_tree = closest_tree_pos_with_height_greater_or_equal_to[tree_height];
            scenic_score[idx] *= (i - closest_tree) as i32;
            closest_tree_pos_with_height_greater_or_equal_to[..=tree_height].fill(i);
        }
    }

    for col in 0..width {
        // north-south
        calculate_score_along_line(
            (col..grid.grid.len()).step_by(width),
            grid,
            &mut scenic_score,
        );
        // south-north
        calculate_score_along_line(
            (col..grid.grid.len()).step_by(width).rev(),
            grid,
            &mut scenic_score,
        );
    }

    for row in 0..height {
        // west-east
        calculate_score_along_line(row * width..(row + 1) * width, grid, &mut scenic_score);
        // south-north
        calculate_score_along_line(
            (row * width..(row + 1) * width).rev(),
            grid,
            &mut scenic_score,
        );
    }

    scenic_score
}

fn star1(input: &TreeGrid) -> usize {
    let visibility = determine_visibility(input);
    visibility.into_iter().filter(|x| *x).count()
}

fn star2(input: &TreeGrid) -> i32 {
    let scenic_scores = determine_scenic_score(input);
    scenic_scores.iter().copied().max().unwrap()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
