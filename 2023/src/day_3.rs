use std::{collections::HashSet, error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_3.txt")?;
    // Part 1
    let symbols = symbols(input.as_str());
    let result: i32 = input
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            numbers_from_line(line)
                .iter()
                .map(|np| (i as i32, *np))
                .collect::<Vec<(i32, (i32, (i32, i32)))>>()
        })
        .filter(|(i, (num, position))| touches_symbol(&symbols, position, i))
        .map(|(_, (num, _))| num)
        .sum();
    // Part 2
    println!("{}", result);
    Ok(())
}

fn touches_symbol(symbols: &HashSet<(i32, i32)>, position: &(i32, i32), line_index: &i32) -> bool {
    let (start, end) = position;
    let top_bottom: Vec<(i32, i32)> = (start - 1..end + 1)
        .flat_map(|x| vec![(line_index - 1, x)])
        .collect();
    top_bottom.iter().any(|p| symbols.contains(p))
        || symbols.contains(&(*line_index, start - 1))
        || symbols.contains(&(*line_index, end + 1))
}
fn symbols(input: &str) -> HashSet<(i32, i32)> {
    input
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.chars()
                .enumerate()
                .filter(|(j, c)| !c.is_digit(10) && *c != '.')
                .map(move |(j, _)| (i as i32, j as i32))
        })
        .collect()
}
fn numbers_from_line(line: &str) -> Vec<(i32, (i32, i32))> {
    line.chars()
        .enumerate()
        .fold(
            (Vec::new(), 0 as i32, 0),
            |(mut acc, mut n, start), (i, c)| {
                if c.is_digit(10) {
                    n *= 10;
                    n += c.to_digit(10).unwrap() as i32;
                    if i == line.len() - 1 {
                        acc.push((n as i32, (start as i32, (i - 1) as i32)));
                    }
                    (acc, n as i32, start as i32)
                } else {
                    if n > 0 {
                        acc.push((n as i32, (start as i32, (i - 1) as i32)));
                    }
                    n = 0;
                    (acc, n as i32, i as i32)
                }
            },
        )
        .0
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_numbers_from_line() {
        let actual = numbers_from_line("123...234...4546");
        assert_eq!(actual, vec![(123, (0, 2)), (234, (5, 8)), (4546, (11, 14))]);
    }

    #[test]
    fn test_symbols_positions() {
        let expected: HashSet<(i32, i32)> =
            vec![(0, 0), (1, 1), (2, 0), (2, 1)].into_iter().collect();
        let actual = symbols("*..\n.^.\n$%9");
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_touches_symbols() {
        let expected: HashSet<(i32, i32)> =
            vec![(0, 0), (1, 1), (2, 0), (2, 1)].into_iter().collect();
        let actual = symbols("*..\n.^.\n$%9");
        assert_eq!(actual, expected);
    }
}
