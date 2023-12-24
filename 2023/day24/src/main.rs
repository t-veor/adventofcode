use std::ops::RangeInclusive;

use nalgebra::{Matrix2, Matrix6, RowVector1, RowVector6, Vector2, Vector3, Vector6};
use utils::read_input_file;

#[derive(Debug, Clone, Copy)]
struct Line {
    start: Vector3<f64>,
    vel: Vector3<f64>,
}

fn xy_intersect_coeffs(u: &Line, v: &Line) -> Option<Vector2<f64>> {
    let a = Matrix2::from_columns(&[
        Vector2::new(u.vel.x, u.vel.y),
        Vector2::new(-v.vel.x, -v.vel.y),
    ]);
    let b = Vector2::new(v.start.x - u.start.x, v.start.y - u.start.y);

    Some(a.try_inverse()? * b)
}

fn xy_intersection(u: &Line, v: &Line) -> Option<(Vector3<f64>, Vector3<f64>)> {
    let coeffs = xy_intersect_coeffs(u, v)?;
    if coeffs.x < 0.0 || coeffs.y < 0.0 {
        None
    } else {
        Some((u.start + coeffs.x * u.vel, v.start + coeffs.y * v.vel))
    }
}

fn parse_input(input: &str) -> Vec<Line> {
    input
        .lines()
        .map(|line| {
            let (p, v) = line.split_once(" @ ").unwrap();
            let p: Vec<_> = p.split(", ").map(|i| i.trim().parse().unwrap()).collect();
            let v: Vec<_> = v.split(", ").map(|i| i.trim().parse().unwrap()).collect();

            Line {
                start: Vector3::new(p[0], p[1], p[2]),
                vel: Vector3::new(v[0], v[1], v[2]),
            }
        })
        .collect()
}

fn count_intersections_in_range(lines: &[Line], xy_range: RangeInclusive<f64>) -> u64 {
    let mut count = 0;

    for i in 0..lines.len() {
        for j in i + 1..lines.len() {
            if let Some((intersect, _)) = xy_intersection(&lines[i], &lines[j]) {
                if xy_range.contains(&intersect.x) && xy_range.contains(&intersect.y) {
                    count += 1;
                }
            }
        }
    }

    count
}

// This, like, almost worked, but floating point inaccuracy screwed me. It would
// be really nice if some matrix math library supported either f128s or
// BigRationals but I couldn't find any!!
/*
fn try_find_intersections(xy_vel: Vector2<f64>, lines: &[Line]) -> Option<Vector3<f64>> {
    // Consider all the points moving in a relative velocity towards a point
    // If this velocity works, they should all move towards the same point

    let xy_vel3 = Vector3::new(xy_vel.x, xy_vel.y, 0.0);

    let mut found_intersection = None::<Vector2<f64>>;

    let time_of_intersection = |l: &Line| {
        let start = l.start;
        let vel = l.vel;
        if vel.x != 0.0 {
            Some(-start.x / vel.x)
        } else if vel.y != 0.0 {
            Some(-start.y / vel.y)
        } else {
            // Useless point
            None
        }
    };

    let mut z_vz = None::<Vector2<f64>>;

    for window in lines.windows(2) {
        let (u, v) = (&window[0], &window[1]);
        let u_prime = Line {
            vel: u.vel - xy_vel3,
            ..*u
        };
        let v_prime = Line {
            vel: v.vel - xy_vel3,
            ..*v
        };
        let (intersection_first, intersection_second) = match xy_intersection(&u_prime, &v_prime) {
            Some((a, b)) => (a, b),
            None => {
                // velocities are parallel, but maybe because they're the same line?
                if u_prime.is_same_xy_line(&v_prime) {
                    // Just skip this pair
                    continue;
                } else {
                    return None;
                }
            }
        };

        let intersection2 = Vector2::new(intersection_first.x, intersection_second.y);

        match found_intersection {
            None => found_intersection = Some(intersection2),
            Some(v) => {
                if nearest_integer_ne(v, intersection2) {
                    // dbg!(v, intersection2);
                    // println!("INTERSECT INCONS");
                    return None;
                }
            }
        }

        let intersection_xy = Vector3::new(intersection_first.x, intersection_first.y, 0.0);

        let t1 = time_of_intersection(&Line {
            start: u.start - intersection_xy,
            vel: u.vel - xy_vel3,
        });
        let t2 = time_of_intersection(&Line {
            start: v.start - intersection_xy,
            vel: v.vel - xy_vel3,
        });
        let (t1, t2) = match (t1, t2) {
            (Some(t1), Some(t2)) => (t1, t2),
            _ => {
                continue;
            }
        };
        // Solve the simultaneous equations:
        // z + t1 v_z = intersection_first.z
        // z + t2 v_z = intersection_second.z
        let m = Matrix2::new(1.0, 1.0, t1, t2);
        let b = Vector2::new(intersection_first.z, intersection_second.z);
        let m_inv = match m.invert() {
            Some(m_inv) => m_inv,
            None => {
                continue;
            }
        };
        let z_candiddate = m_inv * b;

        match z_vz {
            None => z_vz = Some(z_candiddate),
            Some(v) => {
                if nearest_integer_ne(v, z_candiddate) {
                    // dbg!(v, z_candiddate);
                    // panic!("Z INCONS");
                    // return None;
                }
            }
        }

        // dbg!(found_intersection, z_vz);
    }

    let xy = found_intersection?;
    let z = z_vz?.x;

    Some(Vector3::new(xy.x, xy.y, z))
}
*/

fn star1(lines: &[Line]) -> u64 {
    count_intersections_in_range(lines, 200000000000000.0..=400000000000000.0)
}

// Solution for part 2 solved with insight from reddit..
// If we let p0 and v0 be the initial position and velocity of the rock,
// Then for every other hailstone, we have the equation
// p0 + t[i] v0 = p[i] + t[i] v[i]
// Where i is the index of the hailstone, p/v are the initial
// positions/velocities, and t is the time at which they intersect.
// This looks like a set of awful bilinear equations but we can in fact
// simplify:
//
// First, we can rearrange so that
// p0 - p[i] = t[i] (v[i] - v0)
// Which implies that v[i] - v0 is parallel to p[0] - p[i], and therefore:
// (p0 - p[i]) x (v[i] - v0) = 0
// completely eliminating t[i].
//
// We expand this to get
// p0 x v0 - p0 x v[i] - p[i] x v0 + p[i] x v[i] = 0
//
// Now p0 x v0 is the awful bilinear term... but actually, it is the same for
// all i. We can eliminate it if we equate it in two such equations. Let's
// rearrange so it's on one side...
// p0 x v0 = p0 x v[i] + p[i] x v0 - p[i] x v[i]
//
// Now if we pick two indices i and j such that i != j, then:
// p0 x v0 = p0 x v[i] + p[i] x v0 - p[i] x v[i] = p0 x v[j] + p[j] x v0 - p[j] x v[i]
// Now we can eliminate p0 x v0 and simplify:
// p0 x (v[i] - v[j]) + v0 x (p[j] - p[i]) = p[i] x v[i] - p[j] x v[j]
// Then let's let a = v[i] - v[j], b = p[j] - p[i], and c = p[i] x v[i] - p[j] x v[j]:
// p0 x a + v0 x b = c
//
// This therefore gives you 3 linear equations of 6 variables.
// We can repeat this with another pair of (i, j) to get another 3 equations,
// making it possible to solve (I just constructed the matrix Ax = c and
// inverted A).

fn accumulate_rows(l1: &Line, l2: &Line) -> [(RowVector6<f64>, f64); 3] {
    // Constructs the 3 equations for a pair of hailstones
    // p0 x a + v0 x b = c

    let a = l1.vel - l2.vel;
    let b = l2.start - l1.start;
    let c = l1.start.cross(&l1.vel) - l2.start.cross(&l2.vel);

    [
        // Coeffs in order: p0.x, p0.y, p0.z, v0.x, v0.y, v0.z
        (RowVector6::new(0.0, a.z, -a.y, 0.0, b.z, -b.y), c.x),
        (RowVector6::new(-a.z, 0.0, a.x, -b.z, 0.0, b.x), c.y),
        (RowVector6::new(a.y, -a.x, 0.0, b.y, -b.x, 0.0), c.z),
    ]
}

fn solve(lines: &[Line]) -> Option<Vector6<f64>> {
    // Stacks two triples of equations onto each other for a 6x6 matrix A satisfying
    // A x = c
    // (where x = [p0.x, p0.y, p0.z, v0.x, v0.y, v0.z])
    // And then computes x = A^-1 c

    let first = accumulate_rows(&lines[0], &lines[1]);
    let second = accumulate_rows(&lines[1], &lines[2]);

    let a = Matrix6::from_rows(&[
        first[0].0.clone(),
        first[1].0.clone(),
        first[2].0.clone(),
        second[0].0.clone(),
        second[1].0.clone(),
        second[2].0.clone(),
    ]);
    let b = Vector6::from_rows(&[
        RowVector1::new(first[0].1),
        RowVector1::new(first[1].1),
        RowVector1::new(first[2].1),
        RowVector1::new(second[0].1),
        RowVector1::new(second[1].1),
        RowVector1::new(second[2].1),
    ]);

    Some(a.try_inverse()? * b)
}

fn star2(lines: &[Line]) -> i64 {
    let sol = dbg!(solve(lines)).unwrap();
    let mut x = sol.x.round() as i64;
    let mut y = sol.y.round() as i64;
    let mut z = sol.z.round() as i64;
    let vx = *sol.get(3).unwrap() as i64;
    let vy = *sol.get(4).unwrap() as i64;
    let vz = *sol.get(5).unwrap() as i64;

    // Adjust numbers due to floating point inaccuracy
    let l1 = &lines[0];
    let t = ((x as f64 - l1.start.x) / (l1.vel.x - vx as f64)).round() as i64;

    let expected_x = l1.start.x as i64 + l1.vel.x as i64 * t;
    let actual_x = x + vx * t;
    x += expected_x - actual_x;

    let expected_y = l1.start.y as i64 + l1.vel.y as i64 * t;
    let actual_y = y + vy * t;
    y += expected_y - actual_y;

    let expected_z = l1.start.z as i64 + l1.vel.z as i64 * t;
    let actual_z = z + vz * t;
    z += expected_z - actual_z;

    x + y + z
}

fn main() {
    let input = read_input_file!();
    let lines = parse_input(&input);

    println!("{}", star1(&lines));
    println!("{}", star2(&lines));
}
