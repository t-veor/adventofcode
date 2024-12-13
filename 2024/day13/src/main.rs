use utils::{
    glam::{i64vec2, DMat2, I64Vec2},
    read_input_file,
};

#[derive(Debug, Clone)]
struct Machine {
    button_a: I64Vec2,
    button_b: I64Vec2,
    prize: I64Vec2,
}

// This is some half-remembered linear algebra/number theory stuff at 6am.
// I was complicating it significantly:
// * Solved each coordinate as a diophantine equation
// * Used the solution of each diophantine equation to determine two "lines",
//   each of which is a valid series of solutions for each coordinate
// * Found the intersection of the lines
// Instead of, you know, just setting up the linear system and solving explicitly.
#[allow(unused)]
mod old_solve_of_shame {
    use super::Machine;
    use utils::{
        discrete_math::egcd,
        glam::{i64vec2, DMat2, I64Vec2},
    };

    fn max_presses(delta: I64Vec2, target: I64Vec2) -> i64 {
        if delta == I64Vec2::ZERO {
            panic!("Cannot handle delta == 0!");
        } else if delta.x == 0 {
            target.y / delta.y
        } else if delta.y == 0 {
            target.x / delta.x
        } else {
            (target.x / delta.x).min(target.y / delta.y)
        }
    }

    fn backtracking_solver(machine: &Machine) -> Option<(i64, i64)> {
        let Machine {
            button_a,
            button_b,
            prize,
        } = machine.clone();

        assert!(
            button_b.x != 0 && button_b.y != 0,
            "Not prepared to handle button b moving 0 units in either axis!"
        );

        let max_b_presses = max_presses(button_b, prize);

        for b_presses in (0..=max_b_presses).rev() {
            let remainder = prize - b_presses * button_b;
            let max_a_presses = max_presses(button_a, remainder);

            if button_a * max_a_presses == remainder {
                return Some((max_a_presses, b_presses));
            }
        }

        None
    }

    fn solve_simple_diophantine(a: i64, b: i64, c: i64) -> Option<(I64Vec2, I64Vec2)> {
        let (d, e, f) = egcd(a, b);
        if c % d != 0 {
            return None;
        }

        let h = c / d;

        Some((i64vec2(e * h, f * h), i64vec2(b / d, -a / d)))
    }

    #[derive(Debug, Clone, Copy)]
    enum IntIntersection {
        Parallel,
        Fractional,
        Found(I64Vec2),
    }

    fn determinant(x_col: I64Vec2, y_col: I64Vec2) -> i64 {
        x_col.x * y_col.y - x_col.y * y_col.x
    }

    fn solve_machine(machine: &Machine) {
        let Machine {
            button_a,
            button_b,
            prize,
        } = *machine;
    }

    fn linearly_independent(x_col: I64Vec2, y_col: I64Vec2) -> bool {
        determinant(x_col, y_col) != 0
    }

    fn integer_line_intersection(
        line_a: (I64Vec2, I64Vec2),
        line_b: (I64Vec2, I64Vec2),
    ) -> IntIntersection {
        let (r_a, v_a) = line_a;
        let (r_b, v_b) = line_b;

        if !linearly_independent(v_a, -v_b) {
            return IntIntersection::Parallel;
        }

        // Construction Ax = b
        let b = (r_b - r_a).as_dvec2();
        let a = DMat2::from_cols(v_a.as_dvec2(), -v_b.as_dvec2());

        // Solve for x
        let x = a.inverse() * b;
        let x = i64vec2(x.x.round() as i64, x.y.round() as i64);

        // Check that x does in fact solve the equation. If not, then we found an
        // intersection with fractional coordinates
        let intersection_point = r_a + v_a * x.x;
        if intersection_point == r_b + v_b * x.y {
            IntIntersection::Found(intersection_point)
        } else {
            IntIntersection::Fractional
        }
    }

    fn lines_collinear(line_a: (I64Vec2, I64Vec2), line_b: (I64Vec2, I64Vec2)) -> bool {
        let (r_a, v_a) = line_a;
        let (r_b, v_b) = line_b;

        if linearly_independent(v_a, v_b) {
            return false;
        }

        if linearly_independent(v_a, r_b - r_a) {
            return false;
        }

        true
    }

    fn diophantine_solver(machine: &Machine) -> Option<(i64, i64)> {
        let Machine {
            button_a,
            button_b,
            prize,
        } = machine.clone();

        // a.x * button_a.x + a.y * button_b.x = prize.x
        let line_a = solve_simple_diophantine(button_a.x, button_b.x, prize.x)?;
        // b.x * button_a.y + b.y * button_b.y = prize.y
        let line_b = solve_simple_diophantine(button_a.y, button_b.y, prize.y)?;

        match integer_line_intersection(line_a, line_b) {
            IntIntersection::Found(solution) => {
                if solution.x < 0 || solution.y < 0 {
                    None
                } else {
                    Some((solution.x, solution.y))
                }
            }
            IntIntersection::Parallel => {
                if lines_collinear(line_a, line_b) {
                    panic!("Oh god, I don't want to handle this case");
                } else {
                    None
                }
            }
            IntIntersection::Fractional => None,
        }
    }
}

fn parse_input(input: &str) -> Vec<Machine> {
    fn parse_button(line: &str) -> I64Vec2 {
        let (_, coords) = line.split_once(": ").unwrap();
        let (x, y) = coords.split_once(", ").unwrap();
        let (x, y) = (x.strip_prefix('X').unwrap(), y.strip_prefix('Y').unwrap());

        i64vec2(x.parse().unwrap(), y.parse().unwrap())
    }

    fn parse_prize(line: &str) -> I64Vec2 {
        let (_, coords) = line.split_once(": ").unwrap();
        let (x, y) = coords.split_once(", ").unwrap();
        let (x, y) = (x.strip_prefix("X=").unwrap(), y.strip_prefix("Y=").unwrap());

        i64vec2(x.parse().unwrap(), y.parse().unwrap())
    }

    input
        .trim()
        .split("\n\n")
        .map(|section| {
            let lines = section.lines().collect::<Vec<_>>();
            let [a, b, prize] = lines[..] else { panic!() };

            let button_a = parse_button(a);
            let button_b = parse_button(b);
            let prize = parse_prize(prize);

            Machine {
                button_a,
                button_b,
                prize,
            }
        })
        .collect()
}

fn determinant(x_col: I64Vec2, y_col: I64Vec2) -> i64 {
    x_col.x * y_col.y - x_col.y * y_col.x
}

fn explicit_solver(machine: &Machine) -> Option<(i64, i64)> {
    let Machine {
        button_a,
        button_b,
        prize,
    } = *machine;

    if determinant(button_a, button_b) == 0 {
        // So a solution can actually still exist in this case, e.g. prize
        // location is some multiple of button_a/button_b. But the solution to
        // this is actually quite complicated (what if it's neither an integer
        // multiple of button_a nor button_b, but can be obtained using some
        // combination?) and goes into coin change territory...

        // But this situation appears to not actually occur in the input!
        return None;
    }

    // Set up Ax = b
    #[allow(non_snake_case)]
    let A = DMat2::from_cols(button_a.as_dvec2(), button_b.as_dvec2());
    let b = prize.as_dvec2();

    // Solve x = A^-1 b
    let x = A.inverse() * b;
    // Round x to integer coordinates (and check rounding still gives a valid solution)
    let x = x.round().as_i64vec2();

    if x.x * button_a + x.y * button_b == prize {
        // Also need a check for negative button presses...
        if x.x < 0 || x.y < 0 {
            None
        } else {
            Some(x.into())
        }
    } else {
        None
    }
}

fn star1(machines: &[Machine]) -> i64 {
    let mut total_tokens = 0;

    for machine in machines {
        if let Some((a_presses, b_presses)) = explicit_solver(machine) {
            total_tokens += a_presses * 3 + b_presses;
        }
    }

    total_tokens
}

fn star2(machines: &[Machine]) -> i64 {
    let mut total_tokens = 0;

    for machine in machines {
        let mut machine = machine.clone();
        machine.prize.x += 10000000000000;
        machine.prize.y += 10000000000000;

        if let Some((a_presses, b_presses)) = explicit_solver(&machine) {
            total_tokens += a_presses * 3 + b_presses;
        }
    }

    total_tokens
}

fn main() {
    let input = read_input_file!();
    let machines = parse_input(&input);

    println!("{}", star1(&machines));
    println!("{}", star2(&machines));
}
