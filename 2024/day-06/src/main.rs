use std::fs::File;
use std::io::{self, BufRead};
use anyhow::{anyhow, Context};
use ndarray::prelude::*;
use std::collections::HashMap;

fn main() -> anyhow::Result<()> {
    use Direction::*;

    let grid = read_input("example.txt")?;

    // Find start position
    let mut start = grid
        .indexed_iter()
        .find(|(_, &c)| c == '^')
        .ok_or_else(|| anyhow!("Oh no"))?
        .0;

    let mut direction = Up;
    let mut new_start = start;

    let mut visited: HashMap<(usize, usize), Vec<Direction>> = HashMap::new();

    // grid[[6, 3]] = '#';

    'outer: loop {

        // Exit loop if at edge and facing off it
        if match direction {
            Up => new_start.0 == 0,
            Down => new_start.0 == grid.nrows() - 1,
            Left => new_start.1 == 0,
            Right => new_start.1 == grid.ncols() - 1,
        } {
            break;
        }


        // Slice to end of array in the forward direction
        let forward = match direction {
            Up => grid.slice(s![..start.0;-1, start.1]),
            Down => grid.slice(s![start.0 +1.., start.1]),
            Left => grid.slice(s![start.0, ..start.1;-1]),
            Right => grid.slice(s![start.0, start.1 + 1..]),
        };

        for (i, step) in forward.indexed_iter() {

            // Get coords on original grid
            let coords = match direction {
                Up => (start.0 - i - 1, start.1),
                Down => (start.0 + i + 1, start.1),
                Left => (start.0, start.1 - i - 1),
                Right => (start.0, start.1 + i + 1),
            };

            // Loop Detection
            if visited.get(&coords)
                .map_or(false, |directions| directions.contains(&direction)) {
                break 'outer;
            }

            match *step {
                // Walk forward
                '.' | '^' => {
                    visited.entry(coords.clone())
                        .and_modify(|directions| directions.push(direction.clone()))
                        .or_insert_with(|| vec![direction.clone()]);
                    new_start = coords;
                },
                // Turn to the right
                '#' => {
                    start = new_start;
                    direction = match direction {
                        Up => Right,
                        Right => Down,
                        Down => Left,
                        Left => Up,
                    };
                    break;
                },
                _ => return Err(anyhow!("Unexpected character '{}' encountered!", *step)),
            }
        } 
    }

    println!("Visited positions: {}", visited.len());

    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn read_input(filename: &str) -> anyhow::Result<Array2::<char>> {
    let file = File::open(filename).context("Failed to open file")?;
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .collect::<Result<_, _>>()
        .context("Error reading lines")?;

    let rows = lines.len();
    let cols = lines.first().map(|line| line.len()).unwrap_or(0);

    let mut grid = Array2::from_elem((rows, cols), '.');

    for (i, line) in lines.iter().enumerate() {
        for (j, char) in line.chars().enumerate() {
            grid[[i, j]] = char;
        }
    }

    Ok(grid)
}
