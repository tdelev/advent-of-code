use std::{error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_2.txt")?;
    // Part 1
    //let result: u32 = input.lines().map(|line| game_possible(line) as u32).sum();
    // Part 2
    let result: u32 = input.lines().map(|line| game_power_from_line(line)).sum();
    println!("{}", result);
    Ok(())
}

fn game_possible(game: &str) -> u8 {
    let (game, subsets) = game.split_once(": ").unwrap();
    let (_, num) = game.split_once(" ").unwrap();
    let game_num: u8 = num.parse().unwrap();
    let all_possible = possible(to_subsets(subsets));
    if all_possible {
        game_num
    } else {
        0
    }
}

fn game_power_from_line(game: &str) -> u32 {
    let (_, subsets) = game.split_once(": ").unwrap();
    game_power(to_subsets(subsets))
}

fn to_subsets(input: &str) -> Vec<Cube> {
    input
        .split("; ")
        .collect::<Vec<&str>>()
        .iter()
        .flat_map(|s| to_subset(*s))
        .collect()
}

#[derive(Debug, PartialEq)]
enum Cube {
    Red(u8),
    Green(u8),
    Blue(u8),
}

fn to_subset(input: &str) -> Vec<Cube> {
    input
        .split(", ")
        .map(|part| {
            let parts: Vec<&str> = part.split(" ").collect();
            let count: u8 = parts.get(0).unwrap().parse().unwrap();
            let color = parts.get(1).unwrap();
            if *color == "red" {
                Cube::Red(count)
            } else if *color == "green" {
                Cube::Green(count)
            } else {
                Cube::Blue(count)
            }
        })
        .collect()
}

fn possible(subset: Vec<Cube>) -> bool {
    const RED: u8 = 12;
    const GREEN: u8 = 13;
    const BLUE: u8 = 14;
    subset.iter().all(|e| match e {
        Cube::Red(n) => *n <= RED,
        Cube::Green(n) => *n <= GREEN,
        Cube::Blue(n) => *n <= BLUE,
    })
}

fn game_power(colors: Vec<Cube>) -> u32 {
    let max_red = colors
        .iter()
        .max_by_key(|c| match *c {
            Cube::Red(n) => *n,
            _ => 0,
        })
        .unwrap();
    let max_green = colors
        .iter()
        .max_by_key(|c| match *c {
            Cube::Green(n) => *n,
            _ => 0,
        })
        .unwrap();
    let max_blue = colors
        .iter()
        .max_by_key(|c| match *c {
            Cube::Blue(n) => *n,
            _ => 0,
        })
        .unwrap();
    let red = if let Cube::Red(n) = max_red { *n } else { 0 };
    let green = if let Cube::Green(n) = max_green {
        *n
    } else {
        0
    };
    let blue = if let Cube::Blue(n) = max_blue { *n } else { 0 };
    (red as u32) * (green as u32) * (blue as u32)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_should_be_possible() {
        let actual = possible(vec![Cube::Red(12), Cube::Green(13), Cube::Blue(14)]);
        assert_eq!(actual, true);
    }

    #[test]
    fn test_should_be_inpossible() {
        let actual = possible(vec![Cube::Red(13), Cube::Green(13), Cube::Blue(14)]);
        assert_eq!(actual, false);
    }

    #[test]
    fn test_to_subset() {
        let actual = to_subset("2 green, 3 blue, 4 red");
        assert_eq!(actual, vec![Cube::Green(2), Cube::Blue(3), Cube::Red(4)]);
    }

    #[test]
    fn test_game_possible() {
        let actual = game_possible("Game 10: 2 green, 3 blue, 4 red; 10 green, 5 blue");
        assert_eq!(actual, 10);
    }

    #[test]
    fn test_game_inpossible() {
        let actual = game_possible("Game 10: 2 green, 15 blue, 4 red; 10 green, 5 blue");
        assert_eq!(actual, 0);
    }
    //

    #[test]
    fn test_max_each_color_power() {
        let actual = game_power(vec![
            Cube::Red(10),
            Cube::Blue(1),
            Cube::Blue(5),
            Cube::Green(3),
            Cube::Red(4),
            Cube::Green(1),
        ]);
        assert_eq!(actual, 150);
    }

    #[test]
    fn test_game_max_power() {
        let actual = game_possible("Game 10: 2 green, 15 blue, 4 red; 10 green, 5 blue");
        assert_eq!(actual, 0);
    }
}
