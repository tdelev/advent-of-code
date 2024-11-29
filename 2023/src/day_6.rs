use std::{collections::HashSet, convert::TryInto, error::Error, fs, usize};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_6.txt")?;
    // Part 1
    let (times_line, distance_line) = input.split_once("\n").unwrap();
    let times: Vec<u32> = line_to_numbers(times_line);
    let distances = line_to_numbers(distance_line);
    println!("times: {:?}, distance: {:?}", times, distances);
    let result = times
        .into_iter()
        .zip(distances)
        .map(|(time, record)| ways_to_beat(time, record))
        .fold(1, |acc, i| acc * i);
    println!("{}", result);
    // Part 2
    Ok(())
}

fn ways_to_beat(time: u32, record: u32) -> u32 {
    (1..=time)
        .map(|t| (time - t) * t)
        .filter(|d| d > &record)
        .count()
        .try_into()
        .unwrap()
}

fn line_to_numbers(line: &str) -> Vec<u32> {
    line.split_once(":")
        .unwrap()
        .1
        .trim()
        .split_whitespace()
        .map(|n| n.parse().unwrap())
        .collect()
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_ways_to_beat() {
        let actual = ways_to_beat(7, 9);
        assert_eq!(actual, 4);
    }

    #[test]
    fn test_ways_to_beat_two() {
        let actual = ways_to_beat(15, 40);
        assert_eq!(actual, 8);
    }
}
