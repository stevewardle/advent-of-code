use std::fs::File;
use std::io::{self, BufRead};
use anyhow::{anyhow, Context, Result, Error};
use std::str::FromStr;

fn main() -> Result<()> {

    let equations = read_input("input.txt")?;

    let mut total_calibration = 0;
    for equation in equations.iter() {
        if check_equation(&equation, false)? {
            total_calibration += equation.test_value;
        }
    }

    println!("Total Calibration Result: {}", total_calibration);

    total_calibration = 0;
    for equation in equations.iter() {
        if check_equation(&equation, true)? {
            total_calibration += equation.test_value;
        }
    }

    println!("Total Calibration Result (with Concatenation): {}", total_calibration);
    Ok(())
}

fn check_equation(equation:&Equation, concat:bool) -> Result<bool> {

    // If the running result has grown too big we are done
    if equation.numbers[0] > equation.test_value {
        return Ok(false)
    }
    // If we have reached the final total
    if equation.numbers.len() == 1 {
        // And matched the test value - we've found a valid result
        if equation.numbers[0] == equation.test_value {
            return Ok(true)
        } else {
            return Ok(false)
        }
    }

    let mut add_next_equation = equation.clone();
    add_next_equation.numbers[0] = add_next_equation.numbers[0] + add_next_equation.numbers[1];
    add_next_equation.numbers.remove(1);
    if check_equation(&add_next_equation, concat)? {
        return Ok(true)
    }

    let mut multiply_next_equation = equation.clone();
    multiply_next_equation.numbers[0] = multiply_next_equation.numbers[0] * multiply_next_equation.numbers[1];
    multiply_next_equation.numbers.remove(1);
    if check_equation(&multiply_next_equation, concat)? {
        return Ok(true)
    }

    if concat {
        let mut concat_next_equation = equation.clone();
        let concatenated = format!("{}{}", concat_next_equation.numbers[0], concat_next_equation.numbers[1]);
        concat_next_equation.numbers[0] = concatenated.parse::<i64>()?;
        concat_next_equation.numbers.remove(1);
        if check_equation(&concat_next_equation, concat)? {
            return Ok(true)
        }
    }

    Ok(false)
}

#[derive(Clone, Debug)]
struct Equation {
    test_value: i64,
    numbers: Vec<i64>,
}

impl FromStr for Equation {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let (test_value, numbers) = s
            .split_once(':')
            .ok_or_else(|| anyhow!("Bad Equation: {s}"))?;
        let numbers = numbers
                          .split_whitespace()
                          .map(|num| num.parse::<i64>())
                          .collect::<Result<Vec<i64>,_>>();
        Ok(Equation {
            test_value: test_value.parse()?,
            numbers: numbers?,
        })
    }
}
fn read_input(filename: &str) -> anyhow::Result<Vec<Equation>> {
    let file = File::open(filename).context("Failed to open file")?;
    let lines = io::BufReader::new(file).lines();
    
    let mut equations = vec![];
    for line in lines {
        let line = line.context("Error reading line")?;
        match line.parse::<Equation>() {
            Ok(equation) => equations.push(equation),
            Err(e) => eprintln!("Failed to parse Equation: {}. Error: {}", line, e)
        }
    }

    Ok(equations)
}
