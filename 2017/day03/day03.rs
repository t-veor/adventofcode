#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashMap;

fn parse_input(input: &str) -> i32 {
    input.trim().parse().unwrap()
}

fn spiral_coordinates(x: i32) -> (i32, i32) {
    if x <= 1 {
        return (0, 0);
    }

    /*
     * Notice that the bottom right corners of the spiral are all the odd square
     * numbers:
     *
     * 17  16  15  14  13
     * 18   5   4   3  12
     * 19   6  [1]  2  11
     * 20   7   8  [9] 10
     * 21  22  23  24 [25]
     * ...                [49]
     *
     * Which of course makes sense, by the time each bottom right corner is
     * reached the arrangement is an exact square, so there must be a square
     * number of numbers in the grid, and each time we go around we add 2 new
     * columns/rows so whenever we reach the bottom right corner the number of
     * rows/columns is odd.
     *
     * To calculate which revolution of the spiral we're in, we simply need to
     * take the square root and then ceil div by 2, offset by 1:
     */
    let n = (((x - 1) as f64).sqrt() as i32 + 1) / 2;
    /*
     * For revolution n, each side is (2n + 1) long. The spiral must look like
     * this:
     *           2n
     *  <------------------- (2n-1)^2 + 2n + 1 ^
     *  (2n-1)^2 + 4n + 1                      |
     *         |                               | 2n
     *     2n  |                               |
     *         |                        (2n-1)^2 + 1
     *         v (2n-1)^2 + 6n + 1 ----------------> (2n-1)^2 + 8n (=(2n+1)^2))
     *                                    2n
     *
     * So to work out which arm of the spiral we're in, we take x, subtract
     * (2n - 1)^2 + 1, and divide the result by 2n.
     */
    let rev_position = x - (2 * n - 1).pow(2) - 1;
    let rev_arm = rev_position / (2 * n);
    let arm_position = rev_position - rev_arm * 2 * n;
    /*
     * Now that we know which arm we're on, we can calculate the position
     * explicitly. The positions where the arms start look like this:
     *
     *  <------------- (n-1, n) ^
     *  (-n, n-1)               |
     *      |                   |
     *      |                   |
     *      |               (n, -n+1)
     *      v (-n+1, -n) ------------> (n, -n)
     */
    match rev_arm {
        0 => (n, -n + 1 + arm_position),
        1 => (n - 1 - arm_position, n),
        2 => (-n, n - 1 - arm_position),
        3 => (-n + 1 + arm_position, -n),
        _ => unreachable!(),
    }
}

fn star1(input: &i32) -> i32 {
    let (x, y) = spiral_coordinates(*input);
    x.abs() + y.abs()
}

fn star2(input: &i32) -> i32 {
    let mut grid = HashMap::new();
    grid.insert((0, 0), 1);

    for i in 2.. {
        let mut sum = 0;

        let (x, y) = spiral_coordinates(i);
        for dy in -1..=1 {
            for dx in -1..=1 {
                if dx == 0 && dy == 0 {
                    continue;
                }

                let pos = (x + dx, y + dy);
                if let Some(val) = grid.get(&pos) {
                    sum += *val;
                }
            }
        }

        if sum > *input {
            return sum;
        }

        grid.insert((x, y), sum);
    }

    unreachable!()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
