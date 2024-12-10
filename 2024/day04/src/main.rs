use utils::{
    glam::{ivec2, IVec2},
    grid::{CharGrid, DiagDir},
    read_input_file,
};

fn get_word(grid: &CharGrid, mut coord: IVec2, dir: DiagDir, max_len: usize) -> String {
    let mut word = String::new();

    for _ in 0..max_len {
        match grid.get(coord).copied() {
            Some(c) => word.push(c),
            None => break,
        }

        coord = dir.step(coord);
    }

    word
}

fn find_x_mas(grid: &CharGrid, coord: IVec2) -> bool {
    let diag_word = get_word(grid, coord, DiagDir::SouthEast, 3);
    let antidiag_word = get_word(grid, coord + ivec2(0, 2), DiagDir::NorthEast, 3);

    (diag_word == "MAS" || diag_word == "SAM") && (antidiag_word == "MAS" || antidiag_word == "SAM")
}

fn star1(grid: &CharGrid) -> u32 {
    let mut count = 0;

    for coord in grid.coord_iter() {
        if grid.get(coord).copied() == Some('X') {
            for dir in DiagDir::ALL {
                if get_word(grid, coord, dir, 4) == "XMAS" {
                    count += 1;
                }
            }
        }
    }

    count
}

fn star2(grid: &CharGrid) -> u32 {
    let mut count = 0;

    for coord in grid.coord_iter() {
        if find_x_mas(grid, coord) {
            count += 1;
        }
    }

    count
}

fn main() {
    let input = read_input_file!();
    let grid = CharGrid::from_str_chars(&input).unwrap();

    println!("{}", star1(&grid));
    println!("{}", star2(&grid));
}
