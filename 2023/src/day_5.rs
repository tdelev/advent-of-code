use std::{error::Error, fs};
#[derive(Debug)]
struct FromToRange {
    destination: u32,
    source: u32,
    range: u32,
}

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_5.txt")?;
    // Part 1
    let (_, seeds_line) = input.lines().next().unwrap().split_once(":").unwrap();
    let seeds: Vec<u32> = line_to_numbers(seeds_line);
    let mut maps: Vec<Vec<FromToRange>> = Vec::new();
    let mut key: &str = "";
    input.lines().skip(2).for_each(|line| {
        if line.contains(":") {
            key = line.trim_end_matches(":");
            maps.push(Vec::new());
        } else if !line.trim().is_empty() {
            let mappings = maps.last_mut().unwrap();
            let range_values = line_to_numbers(line.trim());
            mappings.push(FromToRange {
                destination: range_values[0],
                source: range_values[1],
                range: range_values[2],
            });
        }
    });
    let result = seeds
        .into_iter()
        .map(|seed| maps.iter().fold(seed, |acc, map| mapping_final(acc, map)))
        .min();
    println!("{:?}", result);
    // Part 2
    Ok(())
}

fn line_to_numbers(line: &str) -> Vec<u32> {
    line.trim().split(" ").map(|n| n.parse().unwrap()).collect()
}

fn mapping(input: u32, from_to: &FromToRange) -> Option<u32> {
    if input >= from_to.source && input <= from_to.source + (from_to.range - 1) {
        Some(from_to.destination + (input - from_to.source))
    } else {
        None
    }
}

fn mapping_final(input: u32, ranges: &Vec<FromToRange>) -> u32 {
    let result = ranges.iter().find_map(|r| mapping(input, r));
    result.unwrap_or(input)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_mapping_none() {
        let actual = mapping(
            79,
            &FromToRange {
                destination: 50,
                source: 98,
                range: 2,
            },
        );
        assert_eq!(actual, None);
    }

    #[test]
    fn test_mappings_ok() {
        let actual = mapping(
            79,
            &FromToRange {
                destination: 52,
                source: 50,
                range: 48,
            },
        );
        assert_eq!(actual, Some(81));
    }

    #[test]
    fn test_mappings_final_found() {
        let mappings = vec![
            FromToRange {
                destination: 50,
                source: 98,
                range: 2,
            },
            FromToRange {
                destination: 52,
                source: 50,
                range: 48,
            },
        ];
        let actual = mapping_final(79, &mappings);
        assert_eq!(actual, 81);
    }

    #[test]
    fn test_mappings_final_not_found() {
        let mappings = vec![
            FromToRange {
                destination: 50,
                source: 98,
                range: 2,
            },
            FromToRange {
                destination: 52,
                source: 50,
                range: 48,
            },
        ];
        let actual = mapping_final(10, &mappings);
        assert_eq!(actual, 10);
    }
}
