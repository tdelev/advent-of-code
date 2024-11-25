use std::{collections::HashSet, convert::TryInto, error::Error, fmt::write, fs, usize};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_3.txt")?;
    // Part 1
    let symbols = symbols(input.as_str());
    let result: u32 = input
        .lines()
        .flat_map(|line| numbers_from_line(line))
        .filter(|(num, (start, end))| true)
        .map(|(num, _)| num)
        .sum();
    // Part 2
    println!("{}", result);
    Ok(())
}

fn touches_symbol(symbols: HashSet<(usize, usize)>, position: (usize, usize)) -> bool {
    let (start, end) = position;
}
fn symbols(input: &str) -> HashSet<(usize, usize)> {
    input
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.chars()
                .enumerate()
                .filter(|(j, c)| !c.is_digit(10) && *c != '.')
                .map(move |(j, _)| (i, j))
        })
        .collect()
}
fn numbers_from_line(line: &str) -> Vec<(u32, (usize, usize))> {
    line.chars()
        .enumerate()
        .fold(
            (Vec::new(), 0, 0 as usize),
            |(mut acc, mut n, start), (i, c)| {
                if c.is_digit(10) {
                    n *= 10;
                    n += c.to_digit(10).unwrap();
                    if i == line.len() - 1 {
                        acc.push((n, (start, i - 1)));
                    }
                    (acc, n, start)
                } else {
                    if n > 0 {
                        acc.push((n, (start, i - 1)));
                    }
                    n = 0;
                    (acc, n, i)
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
        let expected: HashSet<(usize, usize)> =
            vec![(0, 0), (1, 1), (2, 0), (2, 1)].into_iter().collect();
        let actual = symbols("*..\n.^.\n$%9");
        assert_eq!(actual, expected);
    }
}
