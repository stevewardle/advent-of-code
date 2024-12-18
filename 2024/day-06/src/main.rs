use std::fs::File;
use std::io::{self, BufRead};
use anyhow::{anyhow, Context, Result};
use ndarray::prelude::*;
use std::collections::HashMap;

fn main() -> Result<()> {
    use Direction::*;

    let grid = read_input("input.txt")?;

    // Find start position
    let start = grid
        .indexed_iter()
        .find(|(_, &c)| c == '^')
        .ok_or_else(|| anyhow!("Couldn't find start position"))?
        .0;

    // Do the first part
    let visited = match walk_grid(&grid, &start, &Up) {
        Ok(result) => result ,
        Err(e) => return Err(e),
    };
    
    println!("Visited positions: {}", visited.len());


    // Go through each visited position from the original path adding
    // extra obstacles and looking for loops
    let mut modified_grid = grid.clone();
    let mut loops_found = 0;
    for (coords, directions) in visited.iter() {

        // Take the first time a position was encountered only (since
        // places that are hit multiple times might never be hit if a
        // new obstacle is introduced) and walk back one to find the
        // coordinate to start from
        let modified_start = match directions[0] {
            Up => (coords.0 + 1, coords.1),
            Down => (coords.0 - 1, coords.1),
            Left => (coords.0, coords.1 + 1),
            Right => (coords.0, coords.1 - 1),
        };

        // Add the obstacle
        modified_grid[[coords.0, coords.1]] = '#';

        // Walk the path from just before the obstacle looking for loops
        match walk_grid(&modified_grid, &modified_start, &directions[0]) {
            Ok(_) => (),
            Err(e) if e.to_string() == "Loop Detected!" => {
                loops_found += 1;
            }
            Err(e) => return Err(e),
        }

        modified_grid[[coords.0, coords.1]] = '.';
    }

    println!("Obstruction positions resulting in loops: {loops_found}");
    Ok(())
}

fn walk_grid(
    grid: &Array2<char>,
    start: &(usize, usize),
    direction_start: &Direction,)
    -> Result<HashMap<(usize, usize), Vec<Direction>>> {

    use Direction::*;
    let mut direction = direction_start.clone();
    let mut current_coords = start.clone();
    let mut new_coords = current_coords; 

    let mut visited: HashMap<(usize, usize), Vec<Direction>> = HashMap::new();

    loop {

        // Exit loop if at edge and facing off it
        if match direction {
            Up => new_coords.0 == 0,
            Down => new_coords.0 == grid.nrows() - 1,
            Left => new_coords.1 == 0,
            Right => new_coords.1 == grid.ncols() - 1,
        } {
            break;
        }


        // Slice to end of array in the forward direction
        let forward = match direction {
            Up => grid.slice(s![..current_coords.0;-1, current_coords.1]),
            Down => grid.slice(s![current_coords.0 +1.., current_coords.1]),
            Left => grid.slice(s![current_coords.0, ..current_coords.1;-1]),
            Right => grid.slice(s![current_coords.0, current_coords.1 + 1..]),
        };

        for (i, step) in forward.indexed_iter() {

            // Get coords on original grid
            let coords = match direction {
                Up => (current_coords.0 - i - 1, current_coords.1),
                Down => (current_coords.0 + i + 1, current_coords.1),
                Left => (current_coords.0, current_coords.1 - i - 1),
                Right => (current_coords.0, current_coords.1 + i + 1),
            };

            // Loop Detection
            if visited.get(&coords)
                .map_or(false, |directions| directions.contains(&direction)) {
                return Err(anyhow!("Loop Detected!"));
            }

            match *step {
                // Walk forward
                '.' | '^' => {
                    visited.entry(coords.clone())
                        .and_modify(|directions| directions.push(direction.clone()))
                        .or_insert_with(|| vec![direction.clone()]);
                    new_coords = coords;
                },
                // Turn to the right
                '#' => {
                    current_coords = new_coords;
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
    Ok(visited)
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
