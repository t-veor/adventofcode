use std::collections::{HashMap, HashSet, VecDeque};

use utils::{read_input_file, Direction};

#[derive(Debug, Clone)]
struct Grid {
    grid: Vec<Vec<u8>>,
    size: (isize, isize),
}

impl Grid {
    fn get(&self, (x, y): (isize, isize)) -> Option<u8> {
        let (width, height) = self.size;
        if (0..width).contains(&x) && (0..height).contains(&y) {
            Some(self.grid[y as usize][x as usize])
        } else {
            None
        }
    }
}

type PathDag = HashMap<(isize, isize), Vec<((isize, isize), i64)>>;

fn parse_input(input: &str) -> Grid {
    let grid: Vec<_> = input.lines().map(|line| line.as_bytes().to_vec()).collect();
    let width = grid.first().map(|row| row.len()).unwrap_or(0) as isize;
    let height = grid.len() as isize;

    Grid {
        grid,
        size: (width, height),
    }
}

fn compile_graph(grid: &Grid, respect_slopes: bool) -> PathDag {
    let mut dag: PathDag = HashMap::new();

    let mut frontier = VecDeque::new();
    let mut seen = HashSet::new();

    frontier.push_back((1, 0));
    seen.insert((1, 0));

    while let Some(pos) = frontier.pop_front() {
        let possible_moves = allowed_moves(grid, pos, None, respect_slopes);
        for (mut next_pos, mut last_dir) in possible_moves {
            // Step forward until we get an intersection
            let mut path_length = 1;
            loop {
                let next_possible_moves =
                    allowed_moves(grid, next_pos, Some(last_dir), respect_slopes);
                if next_possible_moves.len() != 1 {
                    // Dead end or intersection
                    dag.entry(pos).or_default().push((next_pos, path_length));
                    // Only insert if not a dead end
                    if next_possible_moves.len() > 1 && seen.insert(next_pos) {
                        frontier.push_back(next_pos);
                    }
                    break;
                }

                next_pos = next_possible_moves[0].0;
                last_dir = next_possible_moves[0].1;
                path_length += 1;
            }
        }
    }

    dag
}

fn allowed_moves(
    grid: &Grid,
    pos: (isize, isize),
    last_dir: Option<Direction>,
    respect_slopes: bool,
) -> Vec<((isize, isize), Direction)> {
    let allowed_dirs;
    if let Some(slope_dir) = respect_slopes
        .then_some(grid)
        .and_then(|grid| grid.get(pos))
        .and_then(slope)
    {
        allowed_dirs = vec![slope_dir];
    } else {
        allowed_dirs = Direction::ALL
            .into_iter()
            .filter(|&i| Some(i.flip()) != last_dir)
            .collect()
    };

    let mut allowed_moves = Vec::new();

    for dir in allowed_dirs {
        let next_pos = dir.step(pos);
        match grid.get(next_pos) {
            Some(b'#') | None => (),
            Some(c) => {
                if match slope(c) {
                    Some(slope) if respect_slopes => slope != dir.flip(),
                    _ => true,
                } {
                    allowed_moves.push((next_pos, dir));
                }
            }
        }
    }

    allowed_moves
}

fn slope(c: u8) -> Option<Direction> {
    match c {
        b'^' => Some(Direction::North),
        b'>' => Some(Direction::East),
        b'v' => Some(Direction::South),
        b'<' => Some(Direction::West),
        _ => None,
    }
}

fn invert_dag(dag: &PathDag) -> PathDag {
    let mut inverted_dag = PathDag::new();

    for (&parent, children) in dag {
        for &(child, cost) in children {
            let parents = inverted_dag.entry(child).or_default();
            parents.push((parent, cost));
        }
    }

    inverted_dag
}

fn longest_dag_path(dag: &PathDag, start: (isize, isize), destination: (isize, isize)) -> i64 {
    // Can't be bothered to do a topological sort, just use memoisation instead
    let mut memo = HashMap::new();
    let inverted_dag = invert_dag(dag);

    memo.insert(start, 0);

    fn recurse(
        inverted_dag: &PathDag,
        pos: (isize, isize),
        memo: &mut HashMap<(isize, isize), i64>,
    ) -> i64 {
        if let Some(&length) = memo.get(&pos) {
            return length;
        }

        let length = inverted_dag[&pos]
            .iter()
            .map(|&(parent, cost)| recurse(inverted_dag, parent, memo) + cost)
            .max()
            .unwrap();

        memo.insert(pos, length);
        length
    }

    recurse(&inverted_dag, destination, &mut memo)
}

fn dfs(graph: &PathDag, start: (isize, isize), destination: (isize, isize)) -> i64 {
    fn recurse(
        graph: &PathDag,
        node: (isize, isize),
        destination: (isize, isize),
        length_so_far: i64,
        path: &mut Vec<(isize, isize)>,
    ) -> i64 {
        if node == destination {
            return length_so_far;
        }

        let mut max = 0;
        for &(adjacent, cost) in graph[&node].iter() {
            if path.contains(&adjacent) {
                continue;
            }

            path.push(adjacent);
            let this_length = recurse(graph, adjacent, destination, length_so_far + cost, path);
            if this_length > max {
                max = this_length;
            }
            path.pop();
        }

        max
    }

    recurse(graph, start, destination, 0, &mut Vec::new())
}

fn star1(grid: &Grid) -> i64 {
    let (width, height) = grid.size;
    let dag = compile_graph(grid, true);
    longest_dag_path(&dag, (1, 0), (width - 2, height - 1))
}

fn star2(grid: &Grid) -> i64 {
    // Terrible terrible terrible
    let (width, height) = grid.size;
    let graph = compile_graph(grid, false);
    dfs(&graph, (1, 0), (width - 2, height - 1))
}

fn main() {
    let input = read_input_file!();
    let grid = parse_input(&input);

    println!("{}", star1(&grid));
    println!("{}", star2(&grid));
}
