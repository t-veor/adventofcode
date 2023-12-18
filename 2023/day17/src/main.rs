use utils::{pathfinding::dijkstra, read_input_file, Direction};

#[derive(Debug)]
struct Map {
    width: isize,
    height: isize,
    data: Vec<Vec<u8>>,
}

impl Map {
    fn parse(input: &str) -> Self {
        let data: Vec<Vec<_>> = input
            .lines()
            .map(|line| line.as_bytes().iter().map(|&c| c - b'0').collect())
            .collect();
        let width = data.first().map(|row| row.len()).unwrap_or(0) as isize;
        let height = data.len() as isize;

        Self {
            width,
            height,
            data,
        }
    }

    fn get(&self, pos: (isize, isize)) -> Option<i64> {
        let (x, y) = pos;
        if (0..self.width).contains(&x) && (0..self.height).contains(&y) {
            Some(self.data[y as usize][x as usize] as i64)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CrucibleState {
    pos: (isize, isize),
    last_dir: Option<Direction>,
}

impl CrucibleState {
    fn expand_star1(&self, map: &Map) -> Vec<(i64, Self)> {
        let mut expanded = Vec::with_capacity(6);

        let dirs = match self.last_dir {
            Some(dir) => vec![dir.rotate_cw(), dir.rotate_ccw()],
            None => Direction::ALL.to_vec(),
        };

        for dir in dirs {
            let mut movement_cost = 0;
            for n in 1..=3 {
                let new_pos = dir.step_n(self.pos, n);
                if let Some(heat_loss) = map.get(new_pos) {
                    movement_cost += heat_loss;
                    expanded.push((
                        movement_cost,
                        Self {
                            pos: new_pos,
                            last_dir: Some(dir),
                        },
                    ))
                } else {
                    break;
                }
            }
        }

        expanded
    }

    fn expand_star2(&self, map: &Map) -> Vec<(i64, Self)> {
        let mut expanded = Vec::with_capacity(14);

        let dirs = match self.last_dir {
            Some(dir) => vec![dir.rotate_cw(), dir.rotate_ccw()],
            None => Direction::ALL.to_vec(),
        };

        for dir in dirs {
            let mut movement_cost = 0;
            for n in 1..=10 {
                let new_pos = dir.step_n(self.pos, n);
                if let Some(heat_loss) = map.get(new_pos) {
                    movement_cost += heat_loss;
                    if n >= 4 {
                        expanded.push((
                            movement_cost,
                            Self {
                                pos: new_pos,
                                last_dir: Some(dir),
                            },
                        ))
                    }
                } else {
                    break;
                }
            }
        }

        expanded
    }
}

fn star1(map: &Map) -> i64 {
    let dest_pos = (map.width - 1, map.height - 1);

    dijkstra(
        CrucibleState {
            pos: (0, 0),
            last_dir: None,
        },
        |state| state.pos == dest_pos,
        |state| state.expand_star1(map),
    )
    .unwrap()
    .0
}

fn star2(map: &Map) -> i64 {
    let dest_pos = (map.width - 1, map.height - 1);

    dijkstra(
        CrucibleState {
            pos: (0, 0),
            last_dir: None,
        },
        |state| state.pos == dest_pos,
        |state| state.expand_star2(map),
    )
    .unwrap()
    .0
}

fn main() {
    let input = read_input_file!();
    let map = Map::parse(&input);

    println!("{}", star1(&map));
    println!("{}", star2(&map));
}
