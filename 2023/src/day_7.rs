use std::{
    cmp::{self, Ordering},
    collections::HashMap,
    convert::TryInto,
    error::Error,
    fs,
};

#[derive(Debug, PartialEq, Ord, Eq, PartialOrd, Clone)]
enum Hand {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPairs,
    OnePair,
    HighKard,
}

#[derive(Debug)]
struct HandPoints {
    hand: Hand,
    hand_with_joker: Hand,
    source: String,
    converted: String,
    bid: u32,
}

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_7.txt")?;
    // Part 1
    let mut hands: Vec<HandPoints> = input
        .lines()
        .map(|l| l.split_once(" ").unwrap())
        .map(|(h, bid)| HandPoints {
            hand: hand(h),
            source: h.to_string(),
            converted: h
                .to_string()
                .replace("J", "1")
                .replace("Q", "C")
                .replace("K", "D")
                .replace("A", "E")
                .replace("T", "A"),
            hand_with_joker: hand_with_joker(h),
            bid: bid.parse().unwrap(),
        })
        .collect();
    hands.sort_by(|a, b| {
        let first = b.hand_with_joker.cmp(&a.hand_with_joker);
        if first == Ordering::Equal {
            a.converted.cmp(&b.converted)
        } else {
            first
        }
    });
    //let result: u32 = hands
    //    .iter()
    //    .enumerate()
    //    .map(|(i, hand)| (i as u32 + 1) * hand.bid)
    //    .sum();
    // Part 2
    let result: u32 = hands
        .iter()
        .enumerate()
        .map(|(i, hand)| (i as u32 + 1) * hand.bid)
        .sum();
    for h in hands {
        if h.hand_with_joker != h.hand {
            println!("{} : {:?} -> {:?}", h.source, h.hand, h.hand_with_joker);
        }
    }
    println!("{}", result);
    Ok(())
}

fn hand_with_joker(input: &str) -> Hand {
    let mut jokers: u8 = input
        .chars()
        .filter(|c| *c == 'J')
        .count()
        .try_into()
        .unwrap();
    let input_no_jokers: String = input.chars().filter(|c| *c != 'J').collect();
    let mut hand = hand(input);
    let total_jokers = jokers;
    println!("jokers: {}", jokers);
    println!("hand: {:?}", hand);
    while jokers > 0 {
        hand = match hand {
            Hand::FiveOfAKind => Hand::FiveOfAKind,
            Hand::FourOfAKind => Hand::FiveOfAKind,
            Hand::FullHouse => Hand::FourOfAKind,
            Hand::ThreeOfAKind => {
                if total_jokers == 3 {
                    jokers -= 2;
                }
                Hand::FourOfAKind
            }
            Hand::TwoPairs => Hand::FullHouse,
            Hand::OnePair => {
                if total_jokers == 2 {
                    jokers -= 1;
                }
                Hand::ThreeOfAKind
            }
            Hand::HighKard => Hand::OnePair,
        };
        jokers -= 1;
    }
    hand
}

fn hand(input: &str) -> Hand {
    let map = input.chars().fold(HashMap::new(), |mut acc, c| {
        if acc.contains_key(&c) {
            let count = acc.get_mut(&c).unwrap();
            *count += 1;
            acc
        } else {
            acc.insert(c, 1);
            acc
        }
    });
    if map.len() == 1 {
        Hand::FiveOfAKind
    } else if map.len() == 2 {
        if map.values().any(|v| *v == 4) {
            Hand::FourOfAKind
        } else {
            Hand::FullHouse
        }
    } else if map.len() == 3 {
        if map.values().any(|v| *v == 3) {
            Hand::ThreeOfAKind
        } else {
            Hand::TwoPairs
        }
    } else if map.len() == 4 {
        Hand::OnePair
    } else {
        Hand::HighKard
    }
}
#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_five_of_a_kind() {
        assert_eq!(hand("AAAAA"), Hand::FiveOfAKind);
    }

    #[test]
    fn test_four_of_a_kind() {
        assert_eq!(hand("AAAAQ"), Hand::FourOfAKind);
    }

    #[test]
    fn test_full_house() {
        assert_eq!(hand("AAAQQ"), Hand::FullHouse);
    }

    #[test]
    fn test_theee_of_a_kind() {
        assert_eq!(hand("AAA9Q"), Hand::ThreeOfAKind);
    }

    #[test]
    fn test_two_pairs() {
        assert_eq!(hand("11AQQ"), Hand::TwoPairs);
    }

    #[test]
    fn test_one_pair() {
        assert_eq!(hand("11AKQ"), Hand::OnePair);
    }

    #[test]
    fn test_high_card() {
        assert_eq!(hand("12AKQ"), Hand::HighKard);
    }

    #[test]
    fn test_hand_with_joker() {
        assert_eq!(hand_with_joker("8TTJ7"), Hand::ThreeOfAKind);
    }

    #[test]
    fn test_hand_with_joker_one_pair_jokers() {
        assert_eq!(hand_with_joker("J367J"), Hand::ThreeOfAKind);
    }

    #[test]
    fn test_hand_with_joker_three_of_a_kind_jokers() {
        assert_eq!(hand_with_joker("7JJJ5"), Hand::FourOfAKind);
    }
}
