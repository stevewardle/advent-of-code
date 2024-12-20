use std::fs::File;
use std::io::{self, BufRead};
use anyhow::{Context, Result};
use itertools::Itertools;


fn main() -> Result<()> {

    let (antennae, (max_row, max_col)) = read_input("input.txt")?;

    let antinodes: Vec<(isize, isize)> = antennae
        .iter()
        .combinations(2)
        .filter(|a| (a[0].frequency == a[1].frequency))
        .map(|a| a[0].antinodes(a[1]))
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();

    let mut antinodes: Vec<(isize, isize)> = antinodes
        .iter()
        .filter(|(x, y)| *x >= 0
                      && *x < max_col
                      && *y >= 0
                      && *y < max_row)
        .cloned()
        .collect();

    antinodes.sort();
    antinodes.dedup();
    dbg!(&antinodes);

    println!("Unique Antinode locations: {}", antinodes.len());

    Ok(())
}

#[derive(Clone, Debug)]
struct Antenna {
    frequency: char,
    coords: (isize, isize),
}

impl Antenna {
    fn antinodes(&self, other: &Antenna) -> Result<Vec<(isize, isize)>> {
        let node_delta = (
            self.coords.0 - other.coords.0,
            self.coords.1 - other.coords.1,
        );
        let lower_node = (
            self.coords.0 + node_delta.0,
            self.coords.1 + node_delta.1,
        );
        let upper_node = (
            other.coords.0 - node_delta.0,
            other.coords.1 - node_delta.1,
        );
        let mut antinodes = vec![];
        antinodes.push(lower_node);
        antinodes.push(upper_node);
        Ok(antinodes)
    }
}

fn read_input(filename: &str) -> anyhow::Result<(Vec<Antenna>, (isize, isize))> {
    let file = File::open(filename).context("Failed to open file")?;
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .collect::<Result<_, _>>()
        .context("Error reading lines")?;

    let rows = lines
        .len()
        .try_into()
        .unwrap();

    let cols: isize = lines
        .first()
        .map(|line| line.len())
        .unwrap_or(0)
        .try_into()
        .unwrap();
    
    let mut antennae = vec![];
    for (row, line) in lines.iter().enumerate() {
        for (col, frequency) in line.chars().enumerate() {
           match frequency {
               '.' => (),
               _ => antennae.push(
                   Antenna{
                       frequency: frequency,
                       coords: (row.try_into().unwrap(),
                                col.try_into().unwrap()),
                   }
               ),
           } 
        }
    }
        
    Ok((antennae, (rows, cols)))
}
