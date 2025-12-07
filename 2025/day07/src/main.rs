use utils::{grid::DenseGrid, read_input_file};

fn combined_star1_star2(grid: &DenseGrid<char>) -> (u64, u64) {
    let start_pos = grid
        .items()
        .find(|&(_, c)| *c == 'S')
        .expect("No start pos")
        .0;

    let mut beam_timelines = vec![0u64; grid.width() as _];
    beam_timelines[start_pos.x as usize] = 1;

    let mut times_split = 0u64;

    for row in grid.rows() {
        let mut next_beam_timelines = vec![0; grid.width() as _];

        for (beam_pos, timelines) in beam_timelines.iter().copied().enumerate() {
            if timelines == 0 {
                continue;
            }

            if row[beam_pos] == '^' {
                next_beam_timelines[beam_pos - 1] += timelines;
                next_beam_timelines[beam_pos + 1] += timelines;

                times_split += 1;
            } else {
                next_beam_timelines[beam_pos] += timelines;
            }
        }

        beam_timelines = next_beam_timelines
    }

    (times_split, beam_timelines.iter().sum())
}

fn main() {
    let input = read_input_file!();
    let grid = DenseGrid::read_from_str(&input, |_, c| c).unwrap();

    let (star1, star2) = combined_star1_star2(&grid);

    println!("{star1}");
    println!("{star2}");
}
