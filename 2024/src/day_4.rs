use std::{error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_4.txt")?;
    // Part 1
    let lines: Vec<&str> = input.lines().collect();
    //let horisontal: usize = lines.iter().map(|line| count_xmas(line)).sum();
    //let vertical: usize = (0..lines[0].len())
    //    .map(|i| {
    //        lines
    //            .iter()
    //            .map(|line| line.chars().nth(i).unwrap())
    //            .collect()
    //    })
    //    .map(|line: String| count_xmas(&line))
    //    .sum();
    //let ltru: usize = (0..lines[0].len())
    //    .map(|i| {
    //        lines
    //            .iter()
    //            .enumerate()
    //            .filter(|(j, line)| i + j < line.len())
    //            .map(|(j, line)| line.chars().nth(i + j).unwrap())
    //            .collect()
    //    })
    //    .map(|line: String| count_xmas(&line))
    //    .sum();
    //let ltrd: usize = (0..lines[0].len())
    //    .map(|i| {
    //        lines
    //            .iter()
    //            .skip(1)
    //            .enumerate()
    //            .filter(|(j, _)| *j >= i)
    //            .map(|(j, line)| line.chars().nth(j - i).unwrap())
    //            .collect()
    //    })
    //    .map(|line: String| count_xmas(&line))
    //    .sum();
    //let rtld: usize = (0..lines[0].len())
    //    .map(|i| {
    //        lines
    //            .iter()
    //            .rev()
    //            .enumerate()
    //            .filter(|(j, line)| j + i < line.len())
    //            .map(|(j, line)| line.chars().nth(i + j).unwrap())
    //            .collect()
    //    })
    //    .map(|line: String| count_xmas(&line))
    //    .sum();
    //let rtlu: usize = (0..lines[0].len())
    //    .map(|i| {
    //        lines
    //            .iter()
    //            .rev()
    //            .skip(1)
    //            .enumerate()
    //            .filter(|(j, _)| *j >= i)
    //            .map(|(j, line)| line.chars().nth(j - i).unwrap())
    //            .collect()
    //    })
    //    .map(|line: String| count_xmas(&line))
    //    .sum();
    //println!("{}", horisontal + vertical + ltru + ltrd + rtlu + rtld);

    // Part 2
    let result: usize = (1..(lines.len() - 1))
        .map(|i| {
            let copy = lines.clone();
            lines[i]
                .chars()
                .enumerate()
                .filter(move |(j, c)| {
                    *j > 0 && *j < copy[i].len() - 1 && *c == 'A' && check_mas(&copy, i, *j)
                })
                .count()
        })
        .sum();
    println!("{}", result);
    Ok(())
}

fn count_xmas(line: &str) -> usize {
    line.matches("XMAS").count() + line.matches("SAMX").count()
}

fn check_mas(lines: &Vec<&str>, i: usize, j: usize) -> bool {
    ((char_at(lines, i - 1, j - 1) == 'M' && char_at(lines, i + 1, j + 1) == 'S')
        && (char_at(lines, i - 1, j + 1) == 'M' && char_at(lines, i + 1, j - 1) == 'S'))
        || ((char_at(lines, i - 1, j - 1) == 'M' && char_at(lines, i + 1, j + 1) == 'S')
            && (char_at(lines, i - 1, j + 1) == 'S' && char_at(lines, i + 1, j - 1) == 'M'))
        || ((char_at(lines, i - 1, j - 1) == 'S' && char_at(lines, i + 1, j + 1) == 'M')
            && (char_at(lines, i - 1, j + 1) == 'S' && char_at(lines, i + 1, j - 1) == 'M'))
        || ((char_at(lines, i - 1, j - 1) == 'S' && char_at(lines, i + 1, j + 1) == 'M')
            && (char_at(lines, i - 1, j + 1) == 'M' && char_at(lines, i + 1, j - 1) == 'S'))
}

fn char_at(lines: &Vec<&str>, i: usize, j: usize) -> char {
    lines[i].chars().nth(j).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check_mas_lt_br() {
        let input = vec!["STS", "XXX", "MTM"];
        assert_eq!(check_mas(&input, 1, 1), true);
    }

    #[test]
    fn test_check_mas_rt_bl() {
        let input = vec!["MTS", "XXX", "MTS"];
        assert_eq!(check_mas(&input, 1, 1), true);
    }

    #[test]
    fn test_check_mas_lt_br1() {
        let input = vec!["MTM", "XXX", "STS"];
        assert_eq!(check_mas(&input, 1, 1), true);
    }

    #[test]
    fn test_check_mas_rt_bl1() {
        let input = vec!["STM", "XXX", "STM"];
        assert_eq!(check_mas(&input, 1, 1), true);
    }
}
