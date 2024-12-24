use std::fs::File;
use std::io::Read;
use anyhow::{Context, Result};


fn main() -> Result<()> {

    let disk_map = read_input("input.txt")?;

    // Precompute the length of the compacted blocks
    let final_length = disk_map
        .iter()
        .step_by(2)
        .fold(0, |acc, &x| acc +x);

    // This iterator goes through the array in
    // reverse returning just the sequence of file
    // ids and no gaps
    let mut reverse_iter = disk_map
        .iter()
        .enumerate()
        .filter(|(i,_)| i % 2 == 0)
        .flat_map(|(i,x)| vec![i/2; *x].into_iter())
        .rev();

    // So now we can iterate forwards including the gaps
    // to calculate the checksum as we go
    let checksum = disk_map
        .iter()
        .enumerate()
        .flat_map(|(i,x)| {
            if i % 2 == 0 {
                // For files, write out the ids as usual
                vec![i/2; *x].into_iter()
            } else {
                // For gaps, retrieve the values from the
                // reversed iterator 
                reverse_iter
                    .by_ref()
                    .take(*x)
                    .map(|x| x)
                    .collect::<Vec<_>>()
                    .into_iter()
            }
        })
        // Attach a new iterator to the final array
        .enumerate()
        // And cut it off once we reach the expected max length
        .filter(|(i,_)| *i < final_length)
        .fold(0, |acc, (i,x)| acc + i*x);

    println!("Filesystem Checksum: {checksum}");

    Ok(())
}

fn read_input(filename: &str) -> anyhow::Result<Vec<usize>> {
    let mut file = File::open(filename).context("Failed to open file")?;
    let mut disk_map = String::new();
    file.read_to_string(&mut disk_map)?; 
        
    let disk_map = disk_map
        .chars()
        .filter(|c| c.is_digit(10))
        .map(|c| c.to_digit(10).unwrap() as usize)
        .collect();

    Ok(disk_map)
}
