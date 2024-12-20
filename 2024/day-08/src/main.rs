use std::fs::File;
use std::io::{self, BufRead};
use anyhow::{Context, Result};
use itertools::Itertools;


fn main() -> Result<()> {

    let (antennae, (max_row, max_col)) = read_input("input.txt")?;

    let mut antinodes: Vec<(isize, isize)> = antennae
        .iter()
        .combinations(2)
        .filter(|a| (a[0].frequency == a[1].frequency))
        .map(|a| a[0].antinodes(a[1], (max_row, max_col), false))
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();

    antinodes.sort();
    antinodes.dedup();

    println!("Unique Antinode locations: {}", antinodes.len());

    let mut antinodes_res: Vec<(isize, isize)> = antennae
        .iter()
        .combinations(2)
        .filter(|a| (a[0].frequency == a[1].frequency))
        .map(|a| a[0].antinodes(a[1], (max_row, max_col), true))
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();

    antinodes_res.sort();
    antinodes_res.dedup();

    println!("Unique Antinode locations (with Resonance): {}", antinodes_res.len());

    Ok(())
}

#[derive(Clone, Debug)]
struct Antenna {
    frequency: char,
    coords: (isize, isize),
}

impl Antenna {
    fn antinodes(&self,
                 other: &Antenna,
                 max_coords: (isize, isize),
                 resonance: bool) -> Result<Vec<(isize, isize)>> {

        let mut antinodes = vec![];
        // The case with resonance active includes the nodes
        // themselve as antinodes
        if resonance {
            antinodes.push(self.coords);
            antinodes.push(other.coords);
        }

        // The step between nodes in coord-space
        let node_delta = (
            self.coords.0 - other.coords.0,
            self.coords.1 - other.coords.1,
        );

        // Lower node first, calculate the first offset
        // antinode before the loop
        let mut lower_node = self.coords;
        lower_node.0 += node_delta.0;
        lower_node.1 += node_delta.1;
        
        while lower_node.0 >= 0 && lower_node.0 < max_coords.0
          && lower_node.1 >= 0 && lower_node.1 < max_coords.1 {
            antinodes.push(lower_node);
            // Exit after possibly adding the first antinode
            // if we are doing part 1
            if !resonance { break; };
            lower_node.0 += node_delta.0;
            lower_node.1 += node_delta.1;
        }

        // Now Upper node, calculate the first offset
        // antinode before the loop
        let mut upper_node = other.coords;
        upper_node.0 -= node_delta.0;
        upper_node.1 -= node_delta.1;
        
        while upper_node.0 >= 0 && upper_node.0 < max_coords.0
          && upper_node.1 >= 0 && upper_node.1 < max_coords.1 {
            antinodes.push(upper_node);
            // Exit after possibly adding the first antinode
            // if we are doing part 1
            if !resonance { break; };
            upper_node.0 -= node_delta.0;
            upper_node.1 -= node_delta.1;
        }
        
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
