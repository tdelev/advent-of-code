use std::{error::Error, fs, u32};
#[derive(Debug)]
struct FromToRange {
    destination: u32,
    source: u32,
    range: u32,
}
impl FromToRange {
    fn source_to(&self) -> u32 {
        self.source + self.range - 1
    }

    fn destination_to(&self) -> u32 {
        self.destination + self.range - 1
    }

    fn in_range(&self, input: u32) -> bool {
        input >= self.source && input <= self.destination_to()
    }

    //fn in_range_remaining(&self, input: u32) -> u32 {
    //    if self.in_range(input) {
    //        self.destination_to() - input
    //    } else {
    //
    //    }
    //}

    fn map(&self, input: u32) -> Option<u32> {
        if self.in_range(input) {
            Some(self.destination + (input - self.source))
        } else {
            None
        }
    }
}
fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_5_sample.txt")?;
    // Part 1
    let (_, seeds_line) = input.lines().next().unwrap().split_once(":").unwrap();
    //let seeds: Vec<u32> = line_to_numbers(seeds_line);
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
    // Part 2
    println!("line: {}", seeds_line);
    let seeds = line_to_ranges(seeds_line);
    println!("seeds: {:?}", seeds);
    for seed in seeds {
        println!("min range: {}", min_range(seed, &maps));
    }
    //let result = seeds
    //    .into_iter()
    //    .map(|seed| maps.iter().fold(seed, |acc, map| mapping_final(acc, map)))
    //    .min();
    //println!("{:?}", result);
    // Part 2
    Ok(())
}

fn min_range(range: (u32, u32), ranges: &Vec<Vec<FromToRange>>) -> u32 {
    let mut min = u32::MAX;
    let (start, end) = range;
    println!("start: {start:?}, end: {end:?}");
    for i in start..end {
        for range in ranges {
            let mapping = mapping_final(i, range);
            println!("i: {i:?}, mapping: {mapping:?}");
            if mapping < min {
                min = mapping;
            }
        }
    }
    min
}

fn line_to_numbers(line: &str) -> Vec<u32> {
    line.trim().split(" ").map(|n| n.parse().unwrap()).collect()
}

fn line_to_ranges(line: &str) -> Vec<(u32, u32)> {
    let first: Vec<u32> = line
        .split_ascii_whitespace()
        .enumerate()
        .filter(|(i, _)| i % 2 == 0)
        .map(|(_, n)| n.parse().unwrap())
        .collect();
    let second: Vec<u32> = line
        .split_ascii_whitespace()
        .enumerate()
        .filter(|(i, _)| i % 2 == 1)
        .map(|(_, n)| n.parse().unwrap())
        .collect();
    first
        .into_iter()
        .zip(second.into_iter())
        .map(|(a, b)| (a, a + b))
        .collect()
}

fn mapping_final(input: u32, ranges: &Vec<FromToRange>) -> u32 {
    let result = ranges.iter().find_map(|r| r.map(input));
    result.unwrap_or(input)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_mapping_none() {
        let actual = FromToRange {
            destination: 50,
            source: 98,
            range: 2,
        }
        .map(79);
        assert_eq!(actual, None);
    }

    #[test]
    fn test_mappings_ok() {
        let actual = FromToRange {
            destination: 52,
            source: 50,
            range: 48,
        }
        .map(79);
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

    #[test]
    fn test_line_to_range() {
        let actual = line_to_ranges("2 4 7 3");
        assert_eq!(actual, vec![(2, 6), (7, 10)]);
    }
}
