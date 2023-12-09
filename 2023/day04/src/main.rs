use utils::read_input_file;

#[derive(Debug)]
struct Card {
    winning: Vec<i32>,
    has: Vec<i32>,
}

impl Card {
    fn parse(line: &str) -> Self {
        let (_, line) = line.split_once(": ").unwrap();
        let (winning, has) = line.split_once(" | ").unwrap();
        let winning = winning
            .split_ascii_whitespace()
            .map(|i| i.parse().unwrap())
            .collect();
        let has = has
            .split_ascii_whitespace()
            .map(|i| i.parse().unwrap())
            .collect();

        Self { winning, has }
    }
}

fn parse_input(input: &str) -> Vec<Card> {
    input.lines().map(Card::parse).collect()
}

fn star1(input: &[Card]) -> i32 {
    input
        .iter()
        .map(|card| card.has.iter().filter(|i| card.winning.contains(i)).count())
        .map(|num_matching| {
            if num_matching > 0 {
                1 << (num_matching - 1)
            } else {
                0
            }
        })
        .sum()
}

fn star2(input: &[Card]) -> i32 {
    let mut num_cards = vec![1; input.len()];

    for (i, card) in input.iter().enumerate() {
        let num_this_card = num_cards[i];
        let num_matching = card.has.iter().filter(|i| card.winning.contains(i)).count();

        for j in i + 1..(i + 1 + num_matching).min(num_cards.len()) {
            num_cards[j] += num_this_card;
        }
    }

    num_cards.iter().sum()
}

fn main() {
    let input = read_input_file!();
    let cards = parse_input(&input);

    println!("{}", star1(&cards));
    println!("{}", star2(&cards));
}
