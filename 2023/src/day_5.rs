use std::{
    cmp::{self, min},
    error::Error,
    fs, u64,
};
type Range = (u64, u64);

#[derive(Debug)]
struct FromToRange {
    destination: u64,
    source: u64,
    range: u64,
}
impl FromToRange {
    fn source_to(&self) -> u64 {
        self.source + self.range - 1
    }

    fn destination_to(&self) -> u64 {
        self.destination + self.range - 1
    }

    fn in_range(&self, input: u64) -> bool {
        input >= self.source && input <= self.source_to()
    }

    fn map_range(&self, input: Range) -> Option<Range> {
        let (start, end) = input;
        if self.in_range(start) {
            Some((
                self.destination + (start - self.source),
                min(self.destination + end - self.source, self.destination_to()),
            ))
        } else {
            None
        }
    }

    fn map(&self, input: u64) -> Option<u64> {
        if self.in_range(input) {
            Some(self.destination + (input - self.source))
        } else {
            None
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_5.txt")?;
    // Part 1
    let (_, seeds_line) = input.lines().next().unwrap().split_once(":").unwrap();
    //let seeds: Vec<u64> = line_to_numbers(seeds_line);
    let mut maps: Vec<Vec<FromToRange>> = Vec::new();
    let mut key: &str = "";
    input.lines().skip(2).for_each(|line| {
        if line.contains(":") {
            let mappings = maps.last_mut();
            if let Some(inner) = mappings {
                inner.sort_by_key(|it| it.source);
            }
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
    let mut result: Vec<Range> = Vec::new();
    println!("seeds: {:?}", seeds);
    let mut result_min = u64::MAX;
    for seed in seeds.iter() {
        println!("seed: {:?}", seed);
        let mut ranges = vec![*seed];
        for mappings in maps.iter() {
            //println!("mappings: {:?}", mappings);
            let next = ranges.clone();
            ranges = Vec::new();
            //println!("==========next: {:?}", next);
            for range in next.iter() {
                let local = map_ranges(mappings, *range);
                //println!("range: {:?} -> {:?}", range, local);
                for l in local.into_iter() {
                    ranges.push(l);
                }
            }
            result = ranges.clone();
        }
        let local_min = result.iter().map(|(first, _)| first).min().unwrap();
        result_min = min(result_min, *local_min);
        println!("\nresult: {:?}\n", result);
    }
    println!("{}", result_min);
    //let result = seeds
    //    .into_iter()
    //    .map(|seed| maps.iter().fold(seed, |acc, map| mapping_final(acc, map)))
    //    .min();
    //println!("{:?}", result);
    // Part 2
    Ok(())
}

fn min_range(range: (u64, u64), ranges: &Vec<Vec<FromToRange>>) -> u64 {
    let mut min = u64::MAX;
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

fn line_to_numbers(line: &str) -> Vec<u64> {
    line.trim().split(" ").map(|n| n.parse().unwrap()).collect()
}

fn line_to_ranges(line: &str) -> Vec<(u64, u64)> {
    let first: Vec<u64> = line
        .split_ascii_whitespace()
        .enumerate()
        .filter(|(i, _)| i % 2 == 0)
        .map(|(_, n)| n.parse().unwrap())
        .collect();
    let second: Vec<u64> = line
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

fn mapping_final(input: u64, ranges: &Vec<FromToRange>) -> u64 {
    let result = ranges.iter().find_map(|r| r.map(input));
    result.unwrap_or(input)
}

fn map_ranges(mappings: &Vec<FromToRange>, range: Range) -> Vec<Range> {
    let mut result: Vec<Range> = Vec::new();
    let (mut start, end) = range;
    for mapping in mappings.iter() {
        //println!("start-end: {}-{}", start, end);
        //println!("mapping: {:?}", mapping);
        if let Some((m_start, m_end)) = mapping.map_range((start, end)) {
            //println!("m_start-m_end: {}-{}", m_start, m_end);
            result.push((m_start, m_end));
            let consumed = m_end - m_start + 1;
            //println!("consumed: {:?}", consumed);
            start += consumed;
        }
        if start >= end {
            break;
        }
    }
    if start <= end {
        result.push((start, end));
    }
    result
}

#[cfg(test)]
mod tests {

    use std::vec;

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

    #[test]
    fn test_map_range() {
        let actual = FromToRange {
            destination: 1,
            source: 20,
            range: 10,
        }
        .map_range((20, 25));
        assert_eq!(actual, Some((1, 6)));
    }

    #[test]
    fn test_map_range_single() {
        let actual = FromToRange {
            destination: 1,
            source: 20,
            range: 10,
        }
        .map_range((20, 20));
        assert_eq!(actual, Some((1, 1)));
    }

    #[test]
    fn test_map_range_overflow() {
        let actual = FromToRange {
            destination: 1,
            source: 21,
            range: 15,
        }
        .map_range((25, 50));
        assert_eq!(actual, Some((5, 15)));
    }

    #[test]
    fn test_map_range_missing_start() {
        let actual = FromToRange {
            destination: 1,
            source: 21,
            range: 15,
        }
        .map_range((20, 30));
        assert_eq!(actual, None);
    }

    #[test]
    fn test_map_range_missing_end() {
        let actual = FromToRange {
            destination: 1,
            source: 21,
            range: 15,
        }
        .map_range((40, 50));
        assert_eq!(actual, None);
    }

    #[test]
    fn test_map_range_sample() {
        let actual = FromToRange {
            destination: 52,
            source: 50,
            range: 48,
        }
        .map_range((79, 93));
        assert_eq!(actual, Some((81, 95)));
    }

    #[test]
    fn test_map_ranges() {
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
        let actual = map_ranges(&mappings, (79, 93));
        assert_eq!(actual, vec![(81, 95)]);
    }

    #[test]
    fn test_map_ranges_left() {
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
        let actual = map_ranges(&mappings, (79, 150));
        assert_eq!(actual, vec![(81, 99), (98, 150)]);
    }

    #[test]
    fn test_map_ranges_sample() {
        let mappings = vec![FromToRange {
            destination: 18,
            source: 25,
            range: 70,
        }];
        let actual = map_ranges(&mappings, (81, 95));
        assert_eq!(actual, vec![(74, 87), (95, 95)]);
    }
}
