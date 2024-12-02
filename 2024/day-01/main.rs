use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    let path = "input.txt";

    // Read in the left and right column of location ids
    let mut location_id_l = Vec::new();
    let mut location_id_r = Vec::new();
    if let Ok(lines) = read_lines(path) {
        for line in lines {
            if let Ok(ip) = line {
                let ids: Vec<&str> = ip.split_whitespace().collect();
                if ids.len() == 2 {
                    if let (Ok(id_l), Ok(id_r)) = (ids[0].parse::<i32>(), ids[1].parse::<i32>()) {
                        location_id_l.push(id_l);
                        location_id_r.push(id_r);
                    }
                }
            }
        }
    }

    // Sort into ascending order
    location_id_l.sort();
    location_id_r.sort();

    // Calculate the distances between the two location id columns
    let distances: Vec<i32> = location_id_l.iter()
        .zip(location_id_r.iter())
        .map(|(l, r)| (l - r).abs())
        .collect();

    println!("Total Distance Between Lists: {:?}", distances.iter().sum::<i32>()); 

    // Count the number of times each location id from the left column appears in the right column
    let mut similarity_score = 0;
    for location in location_id_l.into_iter() {
        let count = location_id_r.iter().filter(|x| **x == location).count() as i32;
        similarity_score += location*count;
    }

    println!("Similarity Score: {:?}", similarity_score);

    Ok(())
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
