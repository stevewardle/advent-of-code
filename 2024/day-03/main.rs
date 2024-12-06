use std::fs::read_to_string;
use std::io;

fn main() -> io::Result<()> {
    let path = "input.txt";
    let memory = read_to_string(path)?;
    let pattern = "mul(";

    let mut start = 0;
    let mut instruction_sum = 0;
    while let Some(pos) = memory[start..].find(pattern) {
        let found_pos = start + pos;
        println!("Found {} at {}", pattern, found_pos);
        println!("Memory: {}", &memory[found_pos..found_pos + 20]);
        start = found_pos + pattern.len(); 

        let number1;
        if let Some((num, new_start)) = find_integer_at_position(&memory, start) {
            number1 = num;
            println!("Number1: {}", number1);
            start = new_start;
        } else {
            continue;
        }

        if memory[start..].starts_with(",") {
            start += 1;
        } else {
            continue;
        }

        let number2;
        if let Some((num, new_start)) = find_integer_at_position(&memory, start) {
            number2 = num;
            println!("Number2: {}", number2);
            start = new_start;
        } else {
            continue;
        }

        if memory[start..].starts_with(")") {
            start += 1;
        } else {
            continue;
        }

        instruction_sum += number1 * number2;
    }

    println!("Instruction sum: {}", instruction_sum);

    Ok(())
}

fn find_integer_at_position(s: &str, start: usize) -> Option<(i32, usize)> {
    let mut number_str = String::new();
    let mut end_pos = start;

    for c in s[start..].chars() {
        if c.is_digit(10) {
            number_str.push(c);
            end_pos += c.len_utf8();
        } else {
            break;
        }
    }

    if !number_str.is_empty() {
        if let Ok(number) = number_str.parse::<i32>() {
            return Some((number, end_pos));
        }
    }
    None
}
