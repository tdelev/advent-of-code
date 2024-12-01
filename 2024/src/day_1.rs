use std::{convert::TryInto, error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_1.txt")?;
    // Part 1

    let first: Vec<u32> = input
        .lines()
        .map(|line| line.split_whitespace())
        .map(|mut arr| arr.next().unwrap().parse::<u32>().unwrap())
        .collect();
    //let second: Vec<u32> = input
    //    .lines()
    //    .map(|line| line.split_whitespace())
    //    .map(|mut arr| {
    //        arr.next();
    //        arr.next().unwrap().parse::<u32>().unwrap()
    //    })
    //    .collect();
    //first.sort();
    //second.sort();
    //let result: u32 = first
    //    .iter()
    //    .zip(second.iter())
    //    .map(|(a, b)| a.abs_diff(*b))
    //    .sum();
    //println!("{}", result);
    //let sum: u32 = input.lines().map(|line| number_in_text(line)).sum();
    // Part 2
    let second: Vec<u32> = input
        .lines()
        .map(|line| line.split_whitespace())
        .map(|mut arr| {
            arr.next();
            arr.next().unwrap().parse::<u32>().unwrap()
        })
        .collect();
    let result: u32 = first
        .iter()
        .map(|n| TryInto::<u32>::try_into(second.iter().filter(|s| *s == n).count()).unwrap() * *n)
        .sum();
    println!("{}", result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number_in_text() {}
}
