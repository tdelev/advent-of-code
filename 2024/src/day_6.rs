use std::{collections::HashMap, error::Error, fs, usize};
type Pos = (i32, i32);
fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_6.txt")?;
    let mut map: HashMap<Pos, char> = input
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.chars()
                .enumerate()
                .map(move |(j, c)| ((i as i32, j as i32), c))
        })
        .collect();
    let cm = map.clone();
    let (start, _) = cm.iter().find(|(_, c)| **c != '.' && **c != '#').unwrap();
    let size = cm.keys().max().unwrap();
    walk(&mut map, *start, *size);
    let result = map.values().filter(|c| **c == 'X').count();
    println!("{}", result + 1);
    // Part 1
    //println!("{}", result);
    // Part 2
    Ok(())
}

fn walk(map: &mut HashMap<Pos, char>, pos: Pos, size: Pos) -> () {
    let direction = *map.get(&pos).unwrap();
    let (next, dir) = if direction == '>' {
        ((pos.0, pos.1 + 1), 'v')
    } else if direction == '<' {
        ((pos.0, pos.1 - 1), '^')
    } else if direction == '^' {
        ((pos.0 - 1, pos.1), '>')
    } else {
        ((pos.0 + 1, pos.1), '<')
    };
    let next_pos = if next.0 < 0 || next.0 > size.0 {
        return;
    } else if *map.get(&next).unwrap() == '#' {
        map.entry(pos).and_modify(|e| *e = dir);
        pos
    } else {
        map.entry(pos).and_modify(|e| *e = 'X');
        map.entry(next).and_modify(|e| *e = direction);
        next
    };

    walk(map, next_pos, size);
}

fn print_map(map: &HashMap<Pos, char>, size: Pos) {
    for i in 0..=size.0 {
        for j in 0..=size.1 {
            print!("{}", map[&(i, j)]);
        }
        println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_can_print() {}
}
