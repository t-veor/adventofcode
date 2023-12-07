use std::cmp::{Ordering, Reverse};

use utils::read_input_file;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Card {
    N2 = 0,
    N3,
    N4,
    N5,
    N6,
    N7,
    N8,
    N9,
    T,
    J,
    Q,
    K,
    A,
}

impl Card {
    const COUNT: usize = 13;

    fn parse(c: char) -> Card {
        match c {
            '2' => Card::N2,
            '3' => Card::N3,
            '4' => Card::N4,
            '5' => Card::N5,
            '6' => Card::N6,
            '7' => Card::N7,
            '8' => Card::N8,
            '9' => Card::N9,
            'T' => Card::T,
            'J' => Card::J,
            'Q' => Card::Q,
            'K' => Card::K,
            'A' => Card::A,
            _ => panic!("Unknown card {c}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Part2Card(Card);

impl Part2Card {
    fn part2_rank(self) -> i32 {
        match self.0 {
            Card::J => -1,
            card => card as i32,
        }
    }
}

impl PartialOrd for Part2Card {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.part2_rank().partial_cmp(&other.part2_rank())
    }
}

impl Ord for Part2Card {
    fn cmp(&self, other: &Self) -> Ordering {
        self.part2_rank().cmp(&other.part2_rank())
    }
}

#[derive(Debug, Clone, Copy)]
struct Hand {
    cards: [Card; 5],
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

impl Hand {
    fn parse(str: &str) -> Self {
        Self {
            cards: str
                .chars()
                .map(Card::parse)
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
        }
    }

    fn hand_type(&self) -> HandType {
        let mut freqs = [0; Card::COUNT];
        for &card in self.cards.iter() {
            freqs[card as usize] += 1;
        }
        freqs.sort_unstable_by_key(|&i| Reverse(i));

        match &freqs {
            [5, ..] => HandType::FiveOfAKind,
            [4, ..] => HandType::FourOfAKind,
            [3, 2, ..] => HandType::FullHouse,
            [3, ..] => HandType::ThreeOfAKind,
            [2, 2, ..] => HandType::TwoPair,
            [2, ..] => HandType::OnePair,
            _ => HandType::HighCard,
        }
    }

    fn sort_key(&self) -> (HandType, [Card; 5]) {
        (self.hand_type(), self.cards.clone())
    }

    fn part2_hand_type(&self) -> HandType {
        let mut freqs = [0; Card::COUNT];
        for &card in self.cards.iter() {
            freqs[card as usize] += 1;
        }

        let joker_count = freqs[Card::J as usize];
        freqs[Card::J as usize] = 0;

        freqs.sort_unstable_by_key(|&i| Reverse(i));

        if freqs[0] + joker_count >= 5 {
            HandType::FiveOfAKind
        } else if freqs[0] + joker_count >= 4 {
            HandType::FourOfAKind
        } else if freqs[0] + freqs[1] + joker_count >= 5 {
            // This condition is a little bit weird, but we've established at
            // this point that the hand can't be 5 or 4 of a kind even with
            // jokers. It could still be a full house, so we would want:
            // * freqs[0] + a = 3 (where a is the number of jokers needed to make up a triple)
            // * freqs[1] + b = 2 (where b is the number of jokers needed to make up a pair)
            // * a + b <= joker_count
            // Combining these inequalities actually just gives us the
            // straightforward inequality freqs[0] + freqs[1] + joker_count <= 5.
            HandType::FullHouse
        } else if freqs[0] + joker_count >= 3 {
            HandType::ThreeOfAKind
        } else if freqs[0] + freqs[1] + joker_count >= 4 {
            // Similar logic as above gives us the following inequalities:
            // * freqs[0] + a = 2
            // * freqs[1] + b = 2
            // * a + b <= joker_count
            // Rearranging, this gives freqs[0] + freqs[1] + joker_count >= 4.
            HandType::TwoPair
        } else if freqs[0] + joker_count >= 2 {
            HandType::OnePair
        } else {
            HandType::HighCard
        }
    }

    fn part2_sort_key(&self) -> (HandType, [Part2Card; 5]) {
        (self.part2_hand_type(), self.cards.map(Part2Card))
    }
}

fn parse_input(input: &str) -> Vec<(Hand, i32)> {
    input
        .lines()
        .map(|line| {
            let (hand, bid) = line.split_once(' ').unwrap();
            (Hand::parse(hand), bid.parse().unwrap())
        })
        .collect()
}

fn star1(input: &[(Hand, i32)]) -> i32 {
    let mut sorted_hands = input.to_vec();
    sorted_hands.sort_by_cached_key(|(hand, _)| hand.sort_key());

    sorted_hands
        .iter()
        .enumerate()
        .map(|(i, (_hand, bid))| {
            let rank = (i + 1) as i32;
            rank * bid
        })
        .sum()
}

fn star2(input: &[(Hand, i32)]) -> i32 {
    let mut sorted_hands = input.to_vec();
    sorted_hands.sort_by_cached_key(|(hand, _)| hand.part2_sort_key());

    sorted_hands
        .iter()
        .enumerate()
        .map(|(i, (_hand, bid))| {
            let rank = (i + 1) as i32;
            rank * bid
        })
        .sum()
}

fn main() {
    let input = read_input_file!();
    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
