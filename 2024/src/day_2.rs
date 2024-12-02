use std::{convert::TryInto, error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_2.txt")?;
    // Part 1

    //let result: usize = input
    //    .lines()
    //    .map(|line| line.split(" ").map(|n| n.parse().unwrap()).collect())
    //    .filter(|n: &Vec<i32>| safe(n))
    //    .count();
    //println!("{}", result);

    let result: usize = input
        .lines()
        .map(|line| line.split(" ").map(|n| n.parse().unwrap()).collect())
        .filter(|n: &Vec<i32>| all(n).iter().any(|list| safe(list)))
        .count();
    println!("{}", result);
    // Part 2
    Ok(())
}

fn safe(levels: &Vec<i32>) -> bool {
    let decreasing = (1..levels.len()).all(|i| {
        let diff = levels.get(i - 1).unwrap() - levels.get(i).unwrap();
        diff >= 1 && diff <= 3
    });
    let increasing = (1..levels.len()).all(|i| {
        let diff = levels.get(i).unwrap() - levels.get(i - 1).unwrap();
        diff >= 1 && diff <= 3
    });
    decreasing || increasing
}

fn all(levels: &Vec<i32>) -> Vec<Vec<i32>> {
    (0..levels.len())
        .into_iter()
        .map(|i| {
            let mut copy = levels.clone();
            copy.remove(i);
            copy.clone()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe() {
        assert_eq!(safe(&vec![7, 6, 4, 2, 1]), true);
    }
}
