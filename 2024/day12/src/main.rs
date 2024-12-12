use std::collections::{HashSet, VecDeque};

use utils::{
    glam::IVec2,
    grid::{von_neumann_neighbors, CharGrid, OrthoDir},
    read_input_file,
};

fn flood_fill_region(grid: &CharGrid, start: IVec2) -> HashSet<IVec2> {
    let mut region = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(start);
    region.insert(start);

    let expected_char = *grid.get(start).expect("start point was outside grid");

    while let Some(curr_tile) = queue.pop_front() {
        for neighbor in grid.von_neumann_neighbors(curr_tile) {
            if grid[neighbor] == expected_char {
                if region.insert(neighbor) {
                    queue.push_back(neighbor);
                }
            }
        }
    }

    region
}

fn find_regions(grid: &CharGrid) -> Vec<HashSet<IVec2>> {
    let mut regions = Vec::new();
    let mut uncovered: HashSet<_> = grid.coord_iter().collect();

    while let Some(start_tile) = uncovered.iter().next().copied() {
        let region = flood_fill_region(grid, start_tile);

        for tile in region.iter() {
            uncovered.remove(tile);
        }

        regions.push(region);
    }

    regions
}

fn region_perimeter(region: &HashSet<IVec2>) -> i32 {
    let mut perimeter = 0;

    for tile in region.iter().copied() {
        let neighbor_count = von_neumann_neighbors(tile)
            .into_iter()
            .filter(|neighbor| region.contains(neighbor))
            .count();

        perimeter += 4 - neighbor_count as i32;
    }

    perimeter
}

fn region_side_count(region: &HashSet<IVec2>) -> i32 {
    if region.is_empty() {
        return 0;
    }

    // Collect all "edges" of the region and group by their direction. "Edge" in
    // this case means a length-1 segment of the boundary of the region.

    // Convention - edges always go anticlockwise around enclosed regions.
    // e.g. edges classified as going upwards have the inside on the left and
    // the outside on the right.

    // We represent an edge using the coordinate of the tile on their left-hand
    // side.

    // (Splitting north-pointing edges and south-pointing edges into separate
    // arrays automatically deals with the "crossover" edge case, as at a
    // crossover point edges necessarily meet with opposing directions.)
    let mut edges_by_dir = [const { Vec::new() }; 4];

    for tile in region.iter().copied() {
        for dir in OrthoDir::ALL {
            // Check if there is another tile in the adjacent region.
            if !region.contains(&dir.step(tile)) {
                // If not, there is an edge here, in the direction of dir
                // rotated 90 degrees anticlockwise.
                edges_by_dir[dir.rotate_ccw() as usize].push(tile);
            }
        }
    }

    // Flip the coordinate system of east- and west-pointing edges so all edges
    // can be considered going north-south.
    edges_by_dir[OrthoDir::East.as_index()]
        .iter_mut()
        .for_each(|i| (i.y, i.x) = (i.x, i.y));
    edges_by_dir[OrthoDir::West.as_index()]
        .iter_mut()
        .for_each(|i| (i.y, i.x) = (i.x, i.y));

    // Now, merge all the edges we've collected into sides.
    // Since all edges were flipped to be going north-south, two edges can only
    // be part of the same side if:
    // * They share the same x coordinate
    // * There's a contiguous run of other edges in the same direction that
    //   covers the difference in y coordinates
    // The second condition is tricky to check in one pass, but if the edges are
    // ordered by y coordinate already, we only need to check whether each
    // edge's y coordinate is 1 more than the previous one's.

    let mut sides = 0;
    for mut edges in edges_by_dir {
        // Sort edges by x coordinate and then y coordinate.
        // This has the effect of grouping edges by their x coordinate together
        // in the array, and then putting edges in increasing y-coordinate order
        // within each group. This sets it up to be easy to check the previous
        // two conditions.
        edges.sort_by_key(|i| (i.x, i.y));

        // Now we're ready to merge the edges to count sides.

        // As long as the region was not empty, we have to have at least one
        // edge and side in every direction.
        let mut last = edges[0];
        sides += 1;

        // Then for each edge, it's part of the same side as `last` if:
        // * The x coordinates match
        // * The y coordinate increases by 1
        for &i in edges[1..].iter() {
            if i.x != last.x || i.y != last.y + 1 {
                // Otherwise, we must've started a new side
                sides += 1;
            }

            last = i;
        }
    }

    sides
}

fn parse_input(input: &str) -> CharGrid {
    CharGrid::from_str_chars(&input).unwrap()
}

fn star1(grid: &CharGrid) -> i32 {
    let regions = find_regions(grid);

    let mut total = 0;
    for region in regions {
        let area = region.len() as i32;
        let perimeter = region_perimeter(&region);

        total += area * perimeter;
    }

    total
}

fn star2(grid: &CharGrid) -> i32 {
    let regions = find_regions(grid);

    let mut total = 0;
    for region in regions {
        let area = region.len() as i32;
        let sides = region_side_count(&region);

        total += area * sides;
    }

    total
}

fn main() {
    let input = read_input_file!();
    let grid = parse_input(&input);

    println!("{}", star1(&grid));
    println!("{}", star2(&grid));
}
