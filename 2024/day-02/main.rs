use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    let path = "input.txt";

    let mut count_safe = 0;
    let mut count_safe_dampner = 0;
    if let Ok(lines) = read_lines(path) {
        for line in lines {
            if let Ok(ip) = line {
                // Read levels
                let levels: Vec<i32> = ip.split_whitespace()
                           .map(|s| s.parse().unwrap())
                           .collect();
                
                // Check if the leves are safe
                let is_safe: bool = check_safety(&levels);
                if is_safe {
                    count_safe += 1;
                    count_safe_dampner += 1;
                } else {
                    for i in 0..levels.len() {
                        let mut levels_copy = levels.clone();
                        levels_copy.remove(i);
                        let is_safe: bool = check_safety(&levels_copy);
                        if is_safe {
                            count_safe_dampner += 1;
                            break;
                        }
                    }
                }
            }
        }
    }

    println!("Total safe reports: {:?}", count_safe);
    println!("Total safe reports (Dampner): {:?}", count_safe_dampner);

    Ok(())
}

fn check_safety(levels: &Vec<i32>) -> bool {
    // Get the difference between each level and the next
    let mut levels_shift = levels.clone();
    levels_shift.rotate_left(1);
    let mut levels_diff: Vec<i32> = levels.iter()
                                          .zip(levels_shift.iter())
                                          .map(|(a, b)| a - b)
                                          .collect();
    levels_diff.pop();

    // Check if monotonic and within required range 
    (levels_diff.iter().all(|&x| x > 0) 
    || levels_diff.iter().all(|&x| x < 0))
    && (levels_diff.iter().all(|&x| {x.abs() >= 1 && x.abs() <= 3}))
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
