#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! num_enum = "0.5"
//! ```

use num_enum::{IntoPrimitive, TryFromPrimitive};

// Even from part 1 I can already tell that the input is a cube net. I should
// prepare for the possibility of allowing different sides to be connected to
// each other rather than the simple wrap-around
#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoPrimitive, TryFromPrimitive)]
#[repr(usize)]
enum Direction {
    Right,
    Down,
    Left,
    Up,
}

impl Direction {
    fn delta(&self) -> (isize, isize) {
        match self {
            Direction::Right => (1, 0),
            Direction::Down => (0, 1),
            Direction::Left => (-1, 0),
            Direction::Up => (0, -1),
        }
    }

    fn rotate_cw(self) -> Self {
        self.rotate_cw_by(1)
    }

    fn rotate_ccw(self) -> Self {
        self.rotate_ccw_by(1)
    }

    fn rotate_cw_by(self, x: usize) -> Self {
        ((usize::from(self) + x) % 4).try_into().unwrap()
    }

    fn rotate_ccw_by(self, x: usize) -> Self {
        self.rotate_cw_by(4 - (x % 4))
    }
}

#[derive(Debug, Clone)]
struct Face {
    grid: Vec<bool>,
    size: isize,
    input_position: (isize, isize),
}

impl Face {
    fn get(&self, (x, y): (isize, isize)) -> bool {
        // Just assume the calling function did all the checking for you. This
        // function should get inlined and then the bounds check removed anyway
        self.grid[x as usize + y as usize * self.size as usize]
    }

    #[allow(unused)]
    fn debug_print(&self, cursor_state: Option<(isize, isize)>) {
        println!("Position: {:?}", self.input_position);
        println!("Size: {:?}", self.size);
        for y in 0..self.size {
            for x in 0..self.size {
                if self.get((x, y)) {
                    print!("#")
                } else if cursor_state == Some((x, y)) {
                    print!("!")
                } else {
                    print!(".")
                }
            }
            println!()
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Forward(u32),
    Left,
    Right,
}

fn parse_input(input: String) -> (Vec<Face>, Vec<Instruction>) {
    let lines: Vec<_> = input.lines().collect();
    let face_lines = &lines[..lines.len() - 2];

    // Count the total number of non-zero cells to work out the face size
    let total_cells = face_lines
        .iter()
        .flat_map(|x| x.as_bytes().iter())
        .filter(|&&c| c != b' ')
        .count();
    let cells_per_face = total_cells / 6;
    let face_size = (cells_per_face as f64).sqrt() as usize;

    let mut faces = Vec::with_capacity(6);
    for y in (0..face_lines.len()).step_by(face_size) {
        let top_line = face_lines[y].as_bytes();
        for x in (0..top_line.len()).step_by(face_size) {
            if top_line[x] != b' ' {
                // We've got a face
                let input_position = (x / face_size, y / face_size);
                let mut grid = Vec::with_capacity(face_size * face_size);
                for i in 0..face_size {
                    for j in 0..face_size {
                        grid.push(face_lines[y + i].as_bytes()[x + j] == b'#')
                    }
                }

                faces.push(Face {
                    grid,
                    size: face_size as _,
                    input_position: (input_position.0 as _, input_position.1 as _),
                })
            }
        }
    }

    let mut instrs = Vec::new();
    let raw_instrs = lines.last().unwrap();
    let mut num_start = 0;
    for (i, c) in raw_instrs.as_bytes().iter().enumerate() {
        if !c.is_ascii_digit() {
            if i != num_start {
                instrs.push(Instruction::Forward(
                    raw_instrs[num_start..i].parse().unwrap(),
                ));
            }
            instrs.push(if *c == b'L' {
                Instruction::Left
            } else {
                Instruction::Right
            });
            num_start = i + 1;
        }
    }
    if num_start != raw_instrs.len() {
        instrs.push(Instruction::Forward(
            raw_instrs[num_start..].parse().unwrap(),
        ));
    }

    (faces, instrs)
}

#[derive(Debug, Clone, Copy)]
struct CursorState {
    face: usize,
    position: (isize, isize),
    direction: Direction,
}

#[derive(Debug, Clone, Copy)]
struct Connection {
    to_face: usize,
    cw_rotations: usize,
}

#[derive(Debug)]
struct Map<'a> {
    faces: &'a [Face],
    connectivity: Vec<[Connection; 4]>,
}

impl<'a> Map<'a> {
    fn from_straight(faces: &'a [Face]) -> Self {
        let mut connectivity = Vec::with_capacity(faces.len());

        let max_x = faces
            .iter()
            .map(|face| face.input_position.0 as isize)
            .max()
            .unwrap()
            + 1;
        let max_y = faces
            .iter()
            .map(|face| face.input_position.1 as isize)
            .max()
            .unwrap()
            + 1;

        for face in faces {
            let start_pos = face.input_position;
            let mut connections = Vec::with_capacity(4);
            for i in 0..4 {
                let dir: Direction = i.try_into().unwrap();
                let delta = dir.delta();
                // Find the next face in this direction
                let mut pos = start_pos;
                loop {
                    pos.0 = (pos.0 + delta.0).rem_euclid(max_x);
                    pos.1 = (pos.1 + delta.1).rem_euclid(max_y);
                    if let Some(to_face) = faces.iter().position(|f| f.input_position == pos) {
                        connections.push(Connection {
                            to_face,
                            cw_rotations: 0,
                        });
                        break;
                    }
                }
            }
            connectivity.push(connections.try_into().unwrap());
        }

        Self {
            faces,
            connectivity,
        }
    }

    fn try_move(&self, state: CursorState) -> CursorState {
        let face = &self.faces[state.face];
        let delta = state.direction.delta();
        let next_pos = (state.position.0 + delta.0, state.position.1 + delta.1);

        let next_state = if next_pos.0 >= 0
            && next_pos.1 >= 0
            && next_pos.0 < face.size
            && next_pos.1 < face.size
        {
            // Stayed in the same face, no need for special handling
            CursorState {
                position: next_pos,
                ..state
            }
        } else {
            let mut new_pos = match state.direction {
                Direction::Right => (0, next_pos.1),
                Direction::Down => (next_pos.0, 0),
                Direction::Left => (-1, next_pos.1),
                Direction::Up => (next_pos.0, -1),
            };

            let Connection {
                to_face,
                cw_rotations,
            } = self.connectivity[state.face][usize::from(state.direction)];
            let new_face = &self.faces[to_face];

            if new_pos.0 < 0 {
                new_pos.0 += new_face.size;
            }
            if new_pos.1 < 0 {
                new_pos.1 += new_face.size;
            }

            // Perform the requisite amount of CW rotations
            let mut new_direction = state.direction;
            for _ in 0..cw_rotations {
                new_direction = new_direction.rotate_cw();
                new_pos = (new_face.size - 1 - new_pos.1, new_pos.0);
            }

            CursorState {
                face: to_face,
                position: new_pos,
                direction: new_direction,
            }
        };

        let next_face = &self.faces[next_state.face];
        if next_face.get(next_state.position) {
            state
        } else {
            next_state
        }
    }

    fn score(&self, state: CursorState) -> isize {
        let face = &self.faces[state.face];
        let size = face.size;
        let column = face.input_position.0 * size + state.position.0 + 1;
        let row = face.input_position.1 * size + state.position.1 + 1;

        row * 1000 + column * 4 + usize::from(state.direction) as isize
    }
}

fn star1(input: &(Vec<Face>, Vec<Instruction>)) -> isize {
    let map = Map::from_straight(&input.0);
    let mut state = CursorState {
        face: 0,
        position: (0, 0),
        direction: Direction::Right,
    };

    for instr in &input.1 {
        match instr {
            Instruction::Forward(x) => {
                for _ in 0..*x {
                    state = map.try_move(state);
                }
            }
            Instruction::Left => {
                state.direction = state.direction.rotate_ccw();
            }
            Instruction::Right => {
                state.direction = state.direction.rotate_cw();
            }
        }
    }

    map.score(state)
}

impl<'a> Map<'a> {
    fn from_cube(faces: &'a [Face]) -> Self {
        let mut connectivity = vec![[None::<Connection>; 4]; 6];
        // 12 edges on a cube, x2 because 2 faces share an edge
        let mut connections_remaining = 24;

        // Add initial connections based on the map
        for (i, face) in faces.iter().enumerate() {
            for d in 0..4 {
                let dir: Direction = d.try_into().unwrap();
                let (dx, dy) = dir.delta();
                let target_pos = (face.input_position.0 + dx, face.input_position.1 + dy);

                if let Some(to_face) = faces.iter().position(|f| f.input_position == target_pos) {
                    // Direct connection made
                    connectivity[i][d] = Some(Connection {
                        to_face,
                        cw_rotations: 0,
                    });
                    connections_remaining -= 1;
                }
            }
        }

        let mut connection_made_this_loop;
        while connections_remaining > 0 {
            connection_made_this_loop = false;

            for i in 0..6 {
                // For each face, check every pair of adjacent faces. If they
                // are unconnected, connect them
                for d in 0..4 {
                    let first_direction: Direction = d.try_into().unwrap();
                    let second_direction = first_direction.rotate_cw();

                    let (first_conn, second_conn) =
                        match (connectivity[i][d], connectivity[i][(d + 1) % 4]) {
                            (Some(a), Some(b)) => (a, b),
                            _ => continue,
                        };

                    let first_face = first_conn.to_face;
                    let second_face = second_conn.to_face;

                    let edge_direction_rel_first_face =
                        second_direction.rotate_cw_by(first_conn.cw_rotations);
                    let edge_direction_rel_second_face =
                        first_direction.rotate_cw_by(second_conn.cw_rotations);

                    if connectivity[first_face][usize::from(edge_direction_rel_first_face)]
                        .is_some()
                    {
                        continue;
                    }

                    if connectivity[second_face][usize::from(edge_direction_rel_second_face)]
                        .is_some()
                    {
                        panic!("asymmetrical connection found");
                    }

                    // We can make a connection!
                    let required_cw_rotations =
                        (second_conn.cw_rotations + (4 - first_conn.cw_rotations) + 1) % 4;

                    connectivity[first_face][usize::from(edge_direction_rel_first_face)] =
                        Some(Connection {
                            to_face: second_face,
                            cw_rotations: required_cw_rotations,
                        });
                    connectivity[second_face][usize::from(edge_direction_rel_second_face)] =
                        Some(Connection {
                            to_face: first_face,
                            cw_rotations: (4 - required_cw_rotations) % 4,
                        });
                    connections_remaining -= 2;
                    connection_made_this_loop = true;
                }
            }

            if !connection_made_this_loop {
                panic!("invalid cube net");
            }
        }

        let connectivity = connectivity
            .into_iter()
            .map(|x| [x[0].unwrap(), x[1].unwrap(), x[2].unwrap(), x[3].unwrap()])
            .collect();
        Self {
            faces,
            connectivity,
        }
    }
}

fn star2(input: &(Vec<Face>, Vec<Instruction>)) -> isize {
    let map = Map::from_cube(&input.0);

    let mut state = CursorState {
        face: 0,
        position: (0, 0),
        direction: Direction::Right,
    };

    for instr in &input.1 {
        match instr {
            Instruction::Forward(x) => {
                for _ in 0..*x {
                    state = map.try_move(state);
                }
            }
            Instruction::Left => {
                state.direction = state.direction.rotate_ccw();
            }
            Instruction::Right => {
                state.direction = state.direction.rotate_cw();
            }
        }
    }

    map.score(state)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
