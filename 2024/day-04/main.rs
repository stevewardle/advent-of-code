use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;

fn main() -> io::Result<()> {
    let filename = "input.txt"; 
    let wordsearch = read_file_to_grid(filename)?;

    let word = "XMAS";
    let found = search_grid_for_word(&wordsearch, &word);
    println!("XMAS Appears {} times", found.len());

    let word = "MAS";
    let found = search_grid_for_word_diagonal_only(&wordsearch, &word);

    let mut coord_counts: HashMap<(usize, usize), usize> = HashMap::new();

    for matched in &found {
        let middle_coords = matched[1].coords;
        *coord_counts.entry(middle_coords).or_insert(0) += 1;
    }

    let crosses = coord_counts.values().filter(|&&count| count > 1).count();

    println!("X-MAS crosses appear {} times", crosses);

    Ok(())
}

// Represents a square of the wordsearch, plus its coordinates
// on the **original** grid
#[derive(Clone)]
struct Square {
    letter: char,
    coords: (usize, usize),
}

fn search_grid_for_word_diagonal_only(grid: &Vec<Vec<Square>>, word: &str) -> Vec<Vec<Square>> {
    let mut found: Vec<Vec<Square>> = Vec::new();
    // Left-down-right / Right-up-left diagonal matches
    let grid_skew_left = skew_grid_left(&grid);
    // print_grid(&grid_skew_left);
    found.append(&mut search_rows_for_word(&grid_skew_left, word));

    // Right-down-left / Left-up-right diagonal matches
    let grid_skew_right = skew_grid_right(&grid);
    // print_grid(&grid_skew_right);
    found.append(&mut search_rows_for_word(&grid_skew_right, word));

    found

}

fn search_grid_for_word(grid: &Vec<Vec<Square>>, word: &str) -> Vec<Vec<Square>> {
    let mut found: Vec<Vec<Square>> = Vec::new();
    // print_grid(&grid);
    found.append(&mut search_rows_for_word(&grid, word));

    // Top-bottom matches
    let grid_rotated = rotate_grid(&grid);
    // print_grid(&grid_rotated);
    found.append(&mut search_rows_for_word(&grid_rotated, word));

    // Left-down-right / Right-up-left diagonal matches
    let grid_skew_left = skew_grid_left(&grid);
    // print_grid(&grid_skew_left);
    found.append(&mut search_rows_for_word(&grid_skew_left, word));

    // Right-down-left / Left-up-right diagonal matches
    let grid_skew_right = skew_grid_right(&grid);
    // print_grid(&grid_skew_right);
    found.append(&mut search_rows_for_word(&grid_skew_right, word));

    found

}

fn search_rows_for_word(grid: &Vec<Vec<Square>>, word: &str) -> Vec<Vec<Square>> {
    let mut matches: Vec<Vec<Square>> = Vec::new();

    let word_rev: String = word.chars().rev().collect();

    for i in 0..grid.len() {
        if grid[i].len() >= word.len() {
            for j in 0..grid[i].len() - word.len() + 1 {
                for check in vec![word, &word_rev] {

                    if check.chars().enumerate().all(|(k, c)| grid[i][j + k].letter == c) {
                        let mut matched = Vec::new();
                        for k in 0..word.len() {
                            matched.push(grid[i][j + k].clone());
                        }
                        matches.push(matched);
                    }
                }
            }
        }
    }
    matches
}

// Rotates (transposes) the grid, rows become columns; but note
// that the coordinates stay attached to their square unaltered
fn rotate_grid(grid: &Vec<Vec<Square>>) -> Vec<Vec<Square>> {
    if grid.is_empty() { return Vec::new(); }
    let mut new_grid: Vec<Vec<Square>> = Vec::new(); 
    for _ in grid {
        let new_row: Vec<Square> = Vec::new();
        new_grid.push(new_row);
    }

    for row in grid.iter() {
        for (j, square) in row.iter().enumerate() {
            new_grid[j].push(square.clone());
        }
    }
    new_grid
}

// Skews the grid, such that the rows become diagonal columns
// from lower right to upper left
fn skew_grid_left(grid: &Vec<Vec<Square>>) -> Vec<Vec<Square>> {
    if grid.is_empty() { return Vec::new(); }
    let mut new_grid: Vec<Vec<Square>> = Vec::new(); 
    let rows = grid.len();
    let columns = grid[0].len();
    for _ in 0..rows + columns -1 {
        let new_row: Vec<Square> = Vec::new();
        new_grid.push(new_row);
    }

    for i in 0..rows {
        for j in 0..columns {
            new_grid[i + j].push(grid[i][j].clone());
        }
    }
    new_grid
}

// Skews the grid, such that the rows become diagonal columns
// from upper right to lower left
fn skew_grid_right(grid: &Vec<Vec<Square>>) -> Vec<Vec<Square>> {
    if grid.is_empty() { return Vec::new(); }
    let mut new_grid: Vec<Vec<Square>> = Vec::new(); 
    let rows = grid.len();
    let columns = grid[0].len();
    for _ in 0..rows + columns -1 {
        let new_row: Vec<Square> = Vec::new();
        new_grid.push(new_row);
    }

    for i in 0..rows {
        for j in 0..columns {
            new_grid[columns - j + i - 1].push(grid[i][j].clone());
        }
    }
    new_grid
}

// fn print_grid(grid: &Vec<Vec<Square>>) {
//     for row in grid {
//         for square in row {
//             print!("({},{})={} ", square.coords.0, square.coords.1, square.letter);
//         }
//         println!();
//     }
//     println!();
// }

// fn print_matches(matches: &Vec<Vec<Square>>) {
//     for matched in matches {
//         for square in matched {
//           print!("({},{})={} ", square.coords.0, square.coords.1, square.letter);
//         }
//         println!();
//     }
//     println!();
// }

fn read_file_to_grid(filename: &str) -> io::Result<Vec<Vec<Square>>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);

    let mut grid = Vec::new();

    for (row, line) in reader.lines().enumerate() {
        let line = line?; 
        let mut row_vector: Vec<Square> = Vec::new();
        for (column, character) in line.chars().enumerate() {
            row_vector.push(Square{ letter: character, coords: (row, column)});
        }
        grid.push(row_vector); 
    }

    Ok(grid)
}


