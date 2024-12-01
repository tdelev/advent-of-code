use std::{collections::HashMap, error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_8_sample.txt")?;
    // Part 1
    let instructions: Vec<char> = input.lines().next().unwrap().chars().collect();
    let map: HashMap<_, _> = input
        .lines()
        .skip(2)
        .map(|line| {
            let (source, dest) = line.split_once(" = ").unwrap();
            let (left, right) = dest.split_once(", ").unwrap();
            let value = (left.replace("(", ""), right.replace(")", ""));
            (source, value)
        })
        .collect();
    println!("{:?}", map);
    // Part 2
    Ok(())
}

fn walk(
    map: HashMap<String, (String, String)>,
    instructions: Vec<char>,
    next: char,
    steps: u32,
) -> u32 {
    0
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_five_of_a_kind() {}
}
