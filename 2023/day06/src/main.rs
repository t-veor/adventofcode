use utils::read_input_file;

#[derive(Debug)]
struct Race {
    time: i64,
    distance: i64,
}

impl Race {
    fn count_ways(&self) -> i64 {
        let discriminant = self.time.pow(2) - 4 * self.distance;
        if discriminant < 0 {
            return 0;
        }

        let half_time = self.time as f64 / 2.0;
        let disc_sqrt = (discriminant as f64).sqrt();

        let lower = (half_time - disc_sqrt / 2.0).floor() as i64 + 1;
        let upper = (half_time + disc_sqrt / 2.0).ceil() as i64 - 1;

        upper - lower + 1
    }
}

fn parse_races(input: &str) -> Vec<Race> {
    let mut lines = input.lines();

    let times = lines
        .next()
        .unwrap()
        .strip_prefix("Time:")
        .unwrap()
        .split_ascii_whitespace()
        .map(|i| i.parse::<i64>().unwrap());

    let distances = lines
        .next()
        .unwrap()
        .strip_prefix("Distance:")
        .unwrap()
        .split_ascii_whitespace()
        .map(|i| i.parse::<i64>().unwrap());

    times
        .zip(distances)
        .map(|(time, distance)| Race { time, distance })
        .collect()
}

fn parse_race(input: &str) -> Race {
    let mut lines = input.lines();

    let time = lines
        .next()
        .unwrap()
        .strip_prefix("Time:")
        .unwrap()
        .replace(' ', "")
        .parse()
        .unwrap();

    let distance = lines
        .next()
        .unwrap()
        .strip_prefix("Distance:")
        .unwrap()
        .replace(' ', "")
        .parse()
        .unwrap();

    Race { time, distance }
}

fn star1(input: &str) -> i64 {
    let races = parse_races(input);

    races.iter().map(Race::count_ways).product()
}

fn star2(input: &str) -> i64 {
    let race = parse_race(input);

    race.count_ways()
}

fn main() {
    let input = read_input_file!();

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
