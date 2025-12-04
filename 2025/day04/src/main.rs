use utils::{grid::DenseGrid, read_input_file};

type NeighborGrid = DenseGrid<i8>;

fn parse_input(input: &str) -> NeighborGrid {
    let paper_grid = DenseGrid::read_from_str(input, |_, c| c == '@').unwrap();
    DenseGrid::new(paper_grid.size(), |pos| {
        if paper_grid[pos] {
            paper_grid
                .moore_neighbors(pos)
                .into_iter()
                .filter(|&neighor| paper_grid[neighor])
                .count() as i8
        } else {
            -1
        }
    })
}

fn star1(grid: &NeighborGrid) -> usize {
    grid.values().filter(|&x| (0..4).contains(x)).count()
}

fn star2(mut grid: NeighborGrid) -> usize {
    let mut removable: Vec<_> = grid
        .items()
        .filter_map(|(pos, value)| (0..4).contains(value).then_some(pos))
        .collect();

    let mut removed_count = 0;

    while let Some(to_remove) = removable.pop() {
        if grid[to_remove] < 0 {
            continue;
        }

        grid[to_remove] = -1;
        removed_count += 1;

        for neighbor in grid.moore_neighbors(to_remove) {
            let neighbors_neighbor_count = grid.get_mut(neighbor).unwrap();
            *neighbors_neighbor_count = neighbors_neighbor_count.saturating_sub(1);
            if (0..4).contains(neighbors_neighbor_count) {
                removable.push(neighbor);
            }
        }
    }

    removed_count
}

fn main() {
    let input = read_input_file!();
    let grid = parse_input(&input);

    println!("{}", star1(&grid));
    println!("{}", star2(grid));
}
