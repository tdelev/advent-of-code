use std::{error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_4_sample.txt")?;
    // Part 1
    let lines: Vec<&str> = input.lines().collect();
    let horisontal: usize = lines.iter().map(|line| count_xmas(line)).sum();
    let vertical: usize = (0..lines[0].len())
        .map(|i| {
            lines
                .iter()
                .map(|line| line.chars().nth(i).unwrap())
                .collect()
        })
        .map(|line: String| count_xmas(&line))
        .sum();
    let ltru: usize = (0..lines[0].len())
        .map(|i| {
            lines
                .iter()
                .enumerate()
                .filter(|(j, line)| i + j < line.len())
                .map(|(j, line)| line.chars().nth(i + j).unwrap())
                .collect()
        })
        .map(|line: String| count_xmas(&line))
        .sum();
    let ltrd: usize = (0..lines[0].len())
        .map(|i| {
            lines
                .iter()
                .enumerate()
                .filter(|(j, _)| *j > i)
                .map(|(j, line)| line.chars().nth(j - i).unwrap())
                .collect()
        })
        .map(|line: String| count_xmas(&line))
        .sum();
    let ltrd: usize = (0..lines[0].len())
        .map(|i| {
            lines
                .iter()
                .enumerate()
                .filter(|(j, line)| *j + i < line.len())
                .map(|(j, line)| line.chars().nth(line.len() - i + j - 1).unwrap())
                .collect()
        })
        .map(|line: String| {
            println!("line: {}", line);
            count_xmas(&line)
        })
        .sum();
    //let tbd: usize = (0..lines[0].len()).map(|i| )
    println!("{}", horisontal);
    println!("{}", vertical);
    println!("{}", ltru);
    println!("{}", ltrd);

    // Part 2

    Ok(())
}

fn count_xmas(line: &str) -> usize {
    line.matches("XMAS").count() + line.matches("SAMX").count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe() {}
}
