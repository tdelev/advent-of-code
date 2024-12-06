use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    error::Error,
    fs,
};

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string("input/day_5.txt")?;
    // Part 1
    let (orderings_input, printings) = input.split_once("\n\n").unwrap();
    let after_n: HashMap<i32, HashSet<i32>> = orderings_input
        .split_whitespace()
        .map(|l| {
            let (a, b) = l.split_once("|").unwrap();
            (a.parse::<i32>().unwrap(), b.parse::<i32>().unwrap())
        })
        .into_iter()
        .fold(HashMap::new(), |mut acc, (a, b)| {
            acc.entry(b).or_insert_with(HashSet::new).insert(a);
            acc
        });
    let before_n: HashMap<i32, HashSet<i32>> = orderings_input
        .split_whitespace()
        .map(|l| {
            let (a, b) = l.split_once("|").unwrap();
            (a.parse::<i32>().unwrap(), b.parse::<i32>().unwrap())
        })
        .into_iter()
        .fold(HashMap::new(), |mut acc, (a, b)| {
            acc.entry(a).or_insert_with(HashSet::new).insert(b);
            acc
        });
    //let result: i32 = printings
    //    .lines()
    //    .map(|line| line.split(",").map(|n| n.parse().unwrap()).collect())
    //    .filter(|list| can_print(list, &after_n, &before_n))
    //    .map(|list| *list.get(list.len() / 2).unwrap())
    //    .sum();
    //println!("{}", result);
    // Part 2

    let result: i32 = printings
        .lines()
        .map(|line| line.split(",").map(|n| n.parse().unwrap()).collect())
        .filter(|list| !can_print(list, &after_n, &before_n))
        .map(|mut list| {
            list.sort_by(|a, b| {
                if after_n.get(b).map_or(false, |set| set.contains(a)) {
                    Ordering::Less
                } else if before_n.get(b).map_or(false, |set| set.contains(a)) {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            });
            list
        })
        .map(|list| *list.get(list.len() / 2).unwrap())
        .sum();
    println!("{}", result);
    Ok(())
}

fn can_print(
    list: &Vec<i32>,
    after_n: &HashMap<i32, HashSet<i32>>,
    before_n: &HashMap<i32, HashSet<i32>>,
) -> bool {
    list.iter().enumerate().all(|(i, n)| {
        list.iter().skip(i + 1).all(|after| {
            after_n.get(after).map_or(true, |set| set.contains(n))
                && before_n.get(after).map_or(true, |set| !set.contains(n))
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_can_print() {
        let orderings: HashMap<i32, HashSet<i32>> = HashMap::from([
            (47, HashSet::from([75, 97])),
            (61, HashSet::from([97, 47, 75])),
            (53, HashSet::from([47, 75, 61, 97])),
            (29, HashSet::from([75, 97, 53, 61, 47])),
        ]);
        assert_eq!(
            can_print(&vec![75, 47, 61, 53, 29], &orderings, &HashMap::new()),
            true
        );
    }
}
