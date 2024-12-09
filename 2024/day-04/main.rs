use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let filename = "input.txt"; 
    let word = "XMAS";

    let mut total = 0;

    // Left-right matches
    let wordsearch = read_file_to_grid(filename)?;
    // print_grid(&wordsearch);
    let found = search_for_word(&wordsearch, word);
    // print_matches(&found);
    total += found.len();

    // Top-bottom matches
    let wordsearch_rotated = rotate_grid(&wordsearch);
    // print_grid(&wordsearch_rotated);
    let found = search_for_word(&wordsearch_rotated, word);
    // print_matches(&found);
    total += found.len();

    // Left-down-right / Right-up-left diagonal matches
    let wordsearch_skew_left = skew_grid_left(&wordsearch);
    // print_grid(&wordsearch_skew_left);
    let found = search_for_word(&wordsearch_skew_left, word);
    // print_matches(&found);
    total += found.len();

    // Right-down-left / Left-up-right diagonal matches
    let wordsearch_skew_right = skew_grid_right(&wordsearch);
    // print_grid(&wordsearch_skew_right);
    let found = search_for_word(&wordsearch_skew_right, word);
    // print_matches(&found);
    total += found.len();

    println!("XMAS Appears {} times", total);

    Ok(())
}

// Represents a square of the wordsearch, plus its coordinates
// on the **original** grid
#[derive(Clone)]
struct Square {
    letter: char,
    coords: (usize, usize),
}

fn search_for_word(grid: &Vec<Vec<Square>>, word: &str) -> Vec<Vec<Square>> {
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


