use utils::{read_input_file, Direction};

#[derive(Debug)]
struct DigInstruction {
    dir: Direction,
    moves: usize,
    color: u32,
}

impl DigInstruction {
    fn parse(line: &str) -> Self {
        let parts: Vec<_> = line.split_ascii_whitespace().collect();

        let dir = match parts[0] {
            "U" => Direction::UP,
            "D" => Direction::DOWN,
            "L" => Direction::LEFT,
            "R" => Direction::RIGHT,
            _ => panic!("Unknown direction {}", parts[0]),
        };

        let moves = parts[1].parse().unwrap();

        let color = parts[2]
            .strip_prefix("(#")
            .unwrap()
            .strip_suffix(')')
            .unwrap();
        let color = u32::from_str_radix(color, 16).unwrap();

        Self { dir, moves, color }
    }

    fn extract_from_color(&self) -> Self {
        let moves = self.color >> 4;
        let dir = match self.color % 4 {
            0 => Direction::RIGHT,
            1 => Direction::DOWN,
            2 => Direction::LEFT,
            3 => Direction::UP,
            _ => unreachable!(),
        };

        Self {
            dir,
            moves: moves as _,
            color: 0,
        }
    }
}

fn parse_input(input: &str) -> Vec<DigInstruction> {
    input.lines().map(DigInstruction::parse).collect()
}

fn shoelace_area(path: &[(i64, i64)]) -> i64 {
    (0..path.len())
        .map(|i| (path[i], path[(i + 1) % path.len()]))
        .map(|((x0, y0), (x1, y1))| x0 * y1 - x1 * y0)
        .sum::<i64>()
        .abs()
        / 2
}

fn make_path(instructions: &[DigInstruction]) -> Vec<(i64, i64)> {
    let mut current_pos = (0, 0);
    let mut path = Vec::new();

    for instruction in instructions {
        current_pos = instruction
            .dir
            .step_n(current_pos, instruction.moves as isize);
        path.push((current_pos.0 as i64, current_pos.1 as i64));
    }

    if current_pos != (0, 0) {
        panic!("Path is not closed loop?");
    }

    path
}

fn count_boundary_points(instructions: &[DigInstruction]) -> i64 {
    instructions
        .iter()
        .map(|instruction| instruction.moves as i64)
        .sum()
}

fn star1(instructions: &[DigInstruction]) -> i64 {
    let path = make_path(instructions);
    let boundary_points = count_boundary_points(instructions);

    // The total area we want is actually no. of internal points + no. of
    // boundary points.
    // According to Pick's Theorem, A = I + B/2 - 1
    // Therefore, I + B = A + B/2 + 1
    shoelace_area(&path) + boundary_points / 2 + 1
}

fn star2(instructions: &[DigInstruction]) -> i64 {
    let extracted_instructions: Vec<_> = instructions
        .iter()
        .map(|instruction| instruction.extract_from_color())
        .collect();

    star1(&extracted_instructions)
}

fn main() {
    let input = read_input_file!();
    let instructions = parse_input(&input);

    println!("{}", star1(&instructions));
    println!("{}", star2(&instructions));
}
