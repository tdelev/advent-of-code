use std::{error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_1.txt")?;
    // Part 1
    //let sum: u32 = input.lines().map(|line| number_in_text(line)).sum();
    // Part 2
    let result: u32 = input.lines().map(|line| number_in_text_nat(line)).sum();
    println!("Result: {}", result);
    Ok(())
}

fn number_in_text(text: &str) -> u32 {
    let first_digit = text.chars().find(|c| c.is_digit(10)).unwrap();
    let last_digit = text.chars().rev().find(|c| c.is_digit(10)).unwrap();
    first_digit.to_digit(10).unwrap() * 10 + last_digit.to_digit(10).unwrap()
}

fn number_in_text_nat(text: &str) -> u32 {
    let first = all_substrings(text, true)
        .iter()
        .find_map(|s| find_number_at(*s, true))
        .unwrap();
    let second = all_substrings(text, false)
        .iter()
        .find_map(|s| find_number_at(*s, false))
        .unwrap();
    (first as u32) * 10 + (second as u32)
}

fn find_number_at(text: &str, start: bool) -> Option<usize> {
    let numbers = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];
    let index = if start { 0 } else { text.len() - 1 };
    if text.chars().nth(index).unwrap().is_digit(10) {
        Some(text.chars().nth(index).unwrap().to_digit(10).unwrap() as usize)
    } else {
        numbers
            .iter()
            .enumerate()
            .find(|(_, n)| {
                if start {
                    text.starts_with(*n)
                } else {
                    text.ends_with(*n)
                }
            })
            .map(|(i, _)| i + 1)
    }
}

fn all_substrings(s: &str, start: bool) -> Vec<&str> {
    if start {
        s.char_indices().map(|(i, _)| &s[i..s.len()]).collect()
    } else {
        s.char_indices().map(|(i, _)| &s[0..s.len() - i]).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number_in_text() {
        let actual = number_in_text("abc2ddd3");
        assert_eq!(actual, 23);
    }

    #[test]
    fn test_find_number_at_start() {
        let actual = find_number_at("threefourone33", true);
        assert_eq!(actual, Some(3));
    }

    #[test]
    fn test_find_number_at_start_as_digit() {
        let actual = find_number_at("5three", true);
        assert_eq!(actual, Some(5));
    }

    #[test]
    fn test_find_number_at_end() {
        let actual = find_number_at("threefourone33five", false);
        assert_eq!(actual, Some(5));
    }

    #[test]
    fn test_find_number_at_end_as_digit() {
        let actual = find_number_at("5three4", false);
        assert_eq!(actual, Some(4));
    }

    #[test]
    fn test_number_in_text_nat() {
        let actual = number_in_text_nat("otherfour5six");
        assert_eq!(actual, 46);
    }

    #[test]
    fn test_number_in_text_nat_digit() {
        let actual = number_in_text_nat("other7four5six");
        assert_eq!(actual, 76);
    }
}
