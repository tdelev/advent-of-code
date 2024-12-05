use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    fs,
};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_8.txt")?;
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
    //println!("{:?}", map);
    let result = walk("AAA", map, &instructions);
    println!("{}", result);
    // Part 2
    Ok(())
}

//fn walk(
//    key: &str,
//    map: HashMap<&str, (String, String)>,
//    instructions: &Vec<char>,
//    next: usize,
//    steps: u32,
//) -> u32 {
//    let direction = instructions.get(next).unwrap();
//    let current = map.get(key).unwrap();
//    let dest = if *direction == 'L' {
//        current.0.clone()
//    } else {
//        current.1.clone()
//    };
//    println!("{} : {} -> {} -> {}", steps, key, direction, dest);
//    if dest == "ZZZ" {
//        steps
//    } else {
//        walk(
//            &dest,
//            map,
//            instructions,
//            (next + 1) % instructions.len(),
//            steps + 1,
//        )
//    }
//}

fn walk(key: &str, map: HashMap<&str, (String, String)>, instructions: &Vec<char>) -> u32 {
    let mut next = 0;
    let mut steps = 1;
    let mut next_key = key;
    loop {
        let direction = instructions.get(next).unwrap();
        let current = map.get(next_key).unwrap();
        next_key = if *direction == 'L' {
            &current.0
        } else {
            &current.1
        };
        println!("{} : {} -> {} -> {}", steps, key, direction, next_key);
        if next_key == "ZZZ" {
            return steps;
        } else {
            next = (next + 1) % instructions.len();
        }
        steps += 1;
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_five_of_a_kind() {}
}
