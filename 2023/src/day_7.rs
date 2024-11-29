use std::{collections::HashMap, convert::TryInto, error::Error, fs};

#[derive(Debug, PartialEq)]
enum Hand {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPairs,
    OnePair,
    HighKard,
}

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_7_sample.txt")?;
    // Part 1
    hand("32T3K");
    // Part 2
    Ok(())
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
    println!("map: {:?}", map);
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
}
