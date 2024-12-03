use regex::Regex;
use std::{error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_3.txt")?;
    // Part 1
    //let re = Regex::new(r"mul\(\d{1,3},\d{1,3}\)").unwrap();
    //let result: i32 = re
    //    .find_iter(input.as_str())
    //    .map(|m| m.as_str().replace("mul(", "").replace(")", ""))
    //    .map(|pair| {
    //        let (x, y) = pair.split_once(",").unwrap();
    //        x.parse::<i32>().unwrap() * y.parse::<i32>().unwrap()
    //    })
    //    .sum();
    //println!("{}", result);

    // Part 2

    let re = Regex::new(r"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)").unwrap();
    //re.find_iter(input.as_str())
    //    .for_each(|m| println!("{:?}", m));
    let result: i32 = re
        .find_iter(input.as_str())
        .fold((Vec::new(), true), |mut acc, m| match m.as_str() {
            "do()" => (acc.0, true),
            "don't()" => (acc.0, false),
            _ => {
                if acc.1 {
                    acc.0.push(m.as_str());
                    (acc.0, true)
                } else {
                    acc
                }
            }
        })
        .0
        .iter()
        .map(|m| m.replace("mul(", "").replace(")", ""))
        .map(|pair| {
            let (x, y) = pair.split_once(",").unwrap();
            x.parse::<i32>().unwrap() * y.parse::<i32>().unwrap()
        })
        .sum();
    println!("{:?}", result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe() {}
}
