use utils::read_input_file;

// I know I could make an enum but I just want to able to quickly index into a [i32; 3]
const RED: usize = 0;
const GREEN: usize = 1;
const BLUE: usize = 2;

#[derive(Debug)]
struct Game {
    id: i32,
    rounds: Vec<[i32; 3]>,
}

fn parse_input(input: &str) -> Vec<Game> {
    fn parse_line(line: &str) -> Game {
        let (header, game_rounds) = line.split_once(": ").unwrap();
        let id = header.strip_prefix("Game ").unwrap().parse().unwrap();

        let mut rounds = Vec::new();
        for round in game_rounds.split("; ") {
            let mut round_totals = [0; 3];
            for ball_group in round.split(", ") {
                let (ball_count, ball_color) = ball_group.split_once(" ").unwrap();
                let idx = match ball_color {
                    "red" => RED,
                    "green" => GREEN,
                    "blue" => BLUE,
                    _ => panic!("Invalid color {ball_color}"),
                };

                round_totals[idx] += ball_count.parse::<i32>().unwrap();
            }

            rounds.push(round_totals);
        }

        Game { id, rounds }
    }

    input.lines().map(parse_line).collect()
}

fn star1(games: &[Game]) -> i32 {
    let limit = [12, 13, 14];

    games
        .iter()
        .filter_map(|game| {
            for round in game.rounds.iter() {
                if round
                    .iter()
                    .zip(limit.iter())
                    .any(|(round_balls, limit_balls)| round_balls > limit_balls)
                {
                    return None;
                }
            }
            Some(game.id)
        })
        .sum()
}

fn star2(games: &[Game]) -> i32 {
    games
        .iter()
        .map(|game| {
            let mut maxima = [0; 3];
            for round in game.rounds.iter() {
                round
                    .iter()
                    .enumerate()
                    .for_each(|(i, count)| maxima[i] = maxima[i].max(*count));
            }

            maxima.iter().product::<i32>()
        })
        .sum()
}

fn main() {
    let input = read_input_file!();
    let games = parse_input(&input);

    println!("{}", star1(&games));
    println!("{}", star2(&games));
}
