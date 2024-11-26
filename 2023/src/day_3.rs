use std::{collections::HashSet, error::Error, fs};
type Position = (i32, i32);
type Range = (i32, i32);
type Number = (i32, Range, i32);

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_3.txt")?;
    //let input = fs::read_to_string("input/day_3_sample.txt")?;
    // Part 1
    //let symbols = symbols(input.as_str());
    //let result: i32 = input
    //    .lines()
    //    .enumerate()
    //    .flat_map(|(i, line)| {
    //        numbers_from_line(line)
    //            .iter()
    //            .map(|np| (i as i32, *np))
    //            .collect::<Vec<(i32, Number)>>()
    //    })
    //    .filter(|(line_index, (_, position))| touches_symbol(&symbols, position, line_index))
    //    .map(|(_, (num, _))| num)
    //    .sum();
    // Part 2
    let numbers: Vec<Number> = input
        .lines()
        .enumerate()
        .flat_map(|(i, line)| numbers_from_line(line, i as i32))
        .collect();
    let parts = parts(&input);
    let result: i32 = parts
        .into_iter()
        .map(|part| {
            let touching: Vec<i32> = numbers
                .iter()
                .filter(|number| touches_number(part, number))
                .map(|(n, _, _)| *n)
                .collect();
            if touching.len() == 2 {
                touching.iter().fold(1, |acc, n| acc * n)
            } else {
                0
            }
        })
        .sum();
    println!("{}", result);
    Ok(())
}

fn touches_symbol(symbols: &HashSet<(i32, i32)>, position: &(i32, i32), line_index: &i32) -> bool {
    let (start, end) = position;
    let top_bottom: Vec<(i32, i32)> = (start - 1..=end + 1)
        .flat_map(|x| vec![(line_index - 1, x), (line_index + 1, x)])
        .collect();
    top_bottom.iter().any(|p| symbols.contains(p))
        || symbols.contains(&(*line_index, start - 1))
        || symbols.contains(&(*line_index, end + 1))
}

fn touches_number(part: Position, number: &Number) -> bool {
    let (x, y) = part;
    let (_, (num_y_start, num_y_end), num_x) = *number;
    (x - 1..=x + 1)
        .flat_map(|x| (y - 1..=y + 1).map(move |y| (x, y)))
        .any(|(x, y)| x == num_x && y >= num_y_start && y <= num_y_end)
}

fn symbols(input: &str) -> HashSet<Position> {
    input
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.chars()
                .enumerate()
                .filter(|(_, c)| !c.is_digit(10) && *c != '.')
                .map(move |(j, _)| (i as i32, j as i32))
        })
        .collect()
}

fn parts(input: &str) -> Vec<Position> {
    input
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.chars()
                .enumerate()
                .filter(|(_, c)| *c == '*')
                .map(move |(j, _)| (i as i32, j as i32))
        })
        .collect()
}

fn numbers_from_line(line: &str, row: i32) -> Vec<Number> {
    line.chars()
        .enumerate()
        .fold(
            (Vec::new(), 0i32, 0i32),
            |(mut acc, mut n, start), (i, c)| {
                if c.is_digit(10) {
                    n *= 10;
                    n += c.to_digit(10).unwrap() as i32;
                    if i == line.len() - 1 {
                        acc.push((n, (start, i as i32), row));
                    }
                    (acc, n, start)
                } else {
                    if n > 0 {
                        acc.push((n, (start, i as i32 - 1), row));
                    }
                    n = 0;
                    (acc, n, i as i32 + 1)
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
        let actual = numbers_from_line("123...234...4546", 1);
        assert_eq!(
            actual,
            vec![(123, (0, 2), 1), (234, (6, 8), 1), (4546, (12, 15), 1)]
        );
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

    #[test]
    fn test_touches_number() {
        let actual = touches_number((1, 3), &(35, (2, 3), 2));
        assert_eq!(actual, true);
    }
}
