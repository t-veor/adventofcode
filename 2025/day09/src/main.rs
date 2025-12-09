use std::ops::RangeInclusive;

use utils::{
    glam::{DVec2, dvec2, i64vec2},
    read_input_file,
};

fn parse_input(input: &str) -> Vec<DVec2> {
    input
        .lines()
        .map(|line| {
            let (x, y) = line.split_once(',').unwrap();
            dvec2(x.parse().unwrap(), y.parse().unwrap())
        })
        .collect()
}

fn rectangles(points: &[DVec2]) -> impl Iterator<Item = (DVec2, DVec2)> {
    (0..points.len()).flat_map(move |i| (i + 1..points.len()).map(move |j| (points[i], points[j])))
}

fn star1(points: &[DVec2]) -> i64 {
    rectangles(points)
        .map(|(a, b)| {
            let extent = a - b;
            (extent.abs() + dvec2(1.0, 1.0)).element_product() as i64
        })
        .max()
        .unwrap()
}

struct LineSegments {
    horizontal: Vec<(i64, RangeInclusive<i64>)>,
    vertical: Vec<(i64, RangeInclusive<i64>)>,
}

fn sorted_lines(points: &[DVec2]) -> LineSegments {
    let mut horizontal_lines = Vec::new();
    let mut vertical_lines = Vec::new();

    for i in 0..points.len() {
        let j = (i + 1) % points.len();

        let (a, b) = (points[i], points[j]);
        let a = i64vec2(a.x as _, a.y as _);
        let b = i64vec2(b.x as _, b.y as _);

        if a.x == b.x {
            vertical_lines.push((a.x, a.y.min(b.y)..=a.y.max(b.y)));
        } else {
            horizontal_lines.push((a.y, a.x.min(b.x)..=a.x.max(b.x)));
        }
    }

    horizontal_lines.sort_by_key(|(a, range)| (*a, *range.start(), *range.end()));
    vertical_lines.sort_by_key(|(a, range)| (*a, *range.start(), *range.end()));

    LineSegments {
        horizontal: horizontal_lines,
        vertical: vertical_lines,
    }
}

fn intersects(a: &RangeInclusive<i64>, b: &RangeInclusive<i64>) -> bool {
    a.start() <= b.end() && b.start() <= a.end()
}

fn check_no_stupid_cases_exist(line_segments: &[(i64, RangeInclusive<i64>)]) -> bool {
    let mut stupid_cases_found = false;
    for i in 0..line_segments.len() {
        for j in i + 1..line_segments.len() {
            let (a, b) = (&line_segments[i], &line_segments[j]);

            if a.0.abs_diff(b.0) <= 1 && intersects(&a.1, &b.1) {
                println!("STUPID CASE FOUND: {a:?} {b:?}");
                stupid_cases_found = true;
            }
        }
    }

    stupid_cases_found
}

fn point_in_shape(point: DVec2, line_segments: &LineSegments) -> bool {
    // This function can assume that points in shape are on the integer lattice and point is on the
    // integer lattice + [1/2, 1/2], so that no edge cases with points being the same height as any
    // endpoints will happen
    let mut intersections = 0;

    let start_idx = line_segments
        .vertical
        .partition_point(|segment| point.x > segment.0 as f64);

    for segment in line_segments.vertical.iter().skip(start_idx) {
        let (start, end) = (*segment.1.start() as f64, *segment.1.end() as f64);
        if (start..=end).contains(&point.y) {
            intersections += 1;
        }
    }

    intersections % 2 == 1
}

fn make_half_offset_rectangle(a: DVec2, b: DVec2) -> [DVec2; 4] {
    let left = a.x.min(b.x) + 0.5;
    let right = a.x.max(b.x) - 0.5;
    let top = a.y.min(b.y) + 0.5;
    let bottom = a.y.max(b.y) - 0.5;

    [
        dvec2(left, top),
        dvec2(right, top),
        dvec2(right, bottom),
        dvec2(left, bottom),
    ]
}

fn line_intersections(a: DVec2, b: DVec2, line_segments: &LineSegments) -> bool {
    let (segments_to_consider, partition_range, representative_point) = if a.x == b.x {
        (&line_segments.horizontal, (a.y.min(b.y), a.y.max(b.y)), a.x)
    } else {
        (&line_segments.vertical, (a.x.min(b.x), a.x.max(b.x)), a.y)
    };

    let start_idx =
        segments_to_consider.partition_point(|segment| partition_range.0 > segment.0 as f64);
    let end_idx =
        segments_to_consider.partition_point(|segment| partition_range.1 > segment.0 as f64);

    for segment in &segments_to_consider[start_idx..end_idx] {
        let seg_range = *segment.1.start() as f64..=*segment.1.end() as f64;

        if seg_range.contains(&representative_point) {
            return true;
        }
    }

    false
}

fn is_valid_rectangle(a: DVec2, b: DVec2, line_segments: &LineSegments) -> bool {
    let [a, b, c, d] = make_half_offset_rectangle(a, b);

    point_in_shape(b, line_segments)
        && !(line_intersections(a, b, line_segments)
            || line_intersections(b, c, line_segments)
            || line_intersections(c, d, line_segments)
            || line_intersections(d, a, line_segments))
}

fn star2(points: &[DVec2]) -> i64 {
    let line_segments = sorted_lines(points);
    if check_no_stupid_cases_exist(&line_segments.horizontal)
        || check_no_stupid_cases_exist(&line_segments.vertical)
    {
        println!("Stupid edge case found in the input data, answer for star 2 may not be correct");
    }

    let max_x_extent = points.iter().map(|a| a.x as i64).max().unwrap()
        - points.iter().map(|a| a.x as i64).min().unwrap()
        + 1;
    let max_y_extent = points.iter().map(|a| a.y as i64).max().unwrap()
        - points.iter().map(|a| a.y as i64).min().unwrap()
        + 1;
    let max_1_by_n_rect_size = max_x_extent.max(max_y_extent);

    let result = rectangles(points)
        .filter(|(a, b)| a.x != b.x && a.y != b.y) // Just ignore 1-by-N rectangles
        .filter(|(a, b)| is_valid_rectangle(*a, *b, &line_segments))
        .map(|(a, b)| {
            let extent = a - b;
            (extent.abs() + dvec2(1.0, 1.0)).element_product() as i64
        })
        .max()
        .unwrap();

    if result < max_1_by_n_rect_size {
        println!(
            "Result ({result}) is less than the maximum 1-by-N rect size ({max_1_by_n_rect_size}), answer for star 2 may not be correct"
        );
    }

    result
}

fn main() {
    let input = read_input_file!();
    let points = parse_input(&input);

    println!("{}", star1(&points));
    println!("{}", star2(&points));
}
