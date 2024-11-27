use std::{collections::HashSet, convert::TryInto, error::Error, fs, usize};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_4.txt")?;
    // Part 1
    //let result: u32 = input
    //    .lines()
    //    .map(|line| {
    //        let (_, numbers_part) = line.split_once(": ").unwrap();
    //        let (winning, guesses) = numbers_part.split_once(" | ").unwrap();
    //        let w: HashSet<u32> = numbers(winning.trim()).into_iter().collect();
    //        let h: Vec<u32> = numbers(guesses.trim()).into_iter().collect();
    //        points(w, h)
    //    })
    //    .sum();
    // Part 2
    let mut cards: Vec<u32> = (0..input.lines().count()).map(|_| 0).collect();
    input.lines().enumerate().for_each(|(i, line)| {
        let (_, numbers_part) = line.split_once(": ").unwrap();
        let (winning, guesses) = numbers_part.split_once(" | ").unwrap();
        let w: HashSet<u32> = numbers(winning.trim()).into_iter().collect();
        let h: Vec<u32> = numbers(guesses.trim()).into_iter().collect();
        let c = count(w, h);
        let end: usize = i + (c as usize);
        ((i + 1)..=end).for_each(|j| cards[j] += cards[i] + 1);
        cards[i] += 1;
    });
    println!("cards: {:?}", cards);
    let result: u32 = cards.iter().sum();
    println!("{}", result);
    Ok(())
}

fn numbers(line: &str) -> Vec<u32> {
    line.split(" ")
        .filter(|n| !n.trim().is_empty())
        .into_iter()
        .map(|n| n.parse().unwrap())
        .collect()
}

fn points(winning: HashSet<u32>, hand: Vec<u32>) -> u32 {
    let count = count(winning, hand);
    if count > 0 {
        2u32.pow(count - 1)
    } else {
        0
    }
}

fn count(winning: HashSet<u32>, hand: Vec<u32>) -> u32 {
    hand.iter().filter(|n| winning.contains(n)).count() as u32
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_numbers_from_line() {
        let actual = numbers("1 2 3 4");
        assert_eq!(actual, vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_points() {
        let winning: HashSet<u32> = vec![41, 48, 83, 86, 17].into_iter().collect();
        let hand: Vec<u32> = vec![83, 86, 6, 31, 17, 9, 48, 53];
        let actual = points(winning, hand);
        assert_eq!(actual, 8);
    }
}
