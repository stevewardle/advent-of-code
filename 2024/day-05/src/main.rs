use std::fs::File;
use std::str::FromStr;
use std::io::{self, BufRead};
use anyhow::{anyhow, Context};
use std::collections::HashMap;

fn main() -> anyhow::Result<()> {
    let (page_rules, updates) = read_input("input.txt")?;

    // for rule in page_rules {
    //     println!("{}|{}", rule.low, rule.high);
    // }
    // println!("");
    // for update in updates {
    //     let pages = update.join(",");
    //     println!("{pages}");
    // }

    let mut cache = HashMap::new();
    let mut middle_total = 0;
    'outer: for update in updates {
        for pages in update.windows(2) {
            let current_page = &pages[0];
            let next_page = &pages[1];
            let (lower, _) = cache
                .entry(current_page.clone())
                .or_insert_with(|| get_lower_and_higher(&page_rules, &current_page));
            if lower.contains(next_page) {
                continue 'outer;
            }
        }
        let middle: usize = update[update.len() / 2].parse()?;
        middle_total += middle;
    }
    println!("Total of correctly ordered middle page numbers: {middle_total}");

    Ok(())
}

fn get_lower_and_higher(rules: &[PageRule], target: &str) -> (Vec<String>, Vec<String>) {
    let mut lower = vec![];
    let mut higher = vec![];
    for rule in rules {
        if rule.high == target {
            lower.push(rule.low.clone());
        }
        if rule.low == target {
            higher.push(rule.high.clone());
        }
    }
    (lower, higher)
}

fn read_input(filename: &str) -> anyhow::Result<(Vec<PageRule>, Vec<Vec<String>>)> {
    let file = File::open(filename).context("Failed to open file")?;
    let lines = io::BufReader::new(file).lines();

    let mut page_rules = Vec::new();
    let mut updates = Vec::new();
    let mut after_rules = false;

    for line_result in lines {
        let line = line_result.context("Error reading line")?;
        let trimmed_line = line.trim();

        if trimmed_line.is_empty() {
            after_rules = true;
            continue;
        }

        if after_rules {
            let update: Vec<String> = trimmed_line
                .split(',')
                .map(|s| s.trim().to_string())
                .collect();
            updates.push(update);
        } else {
            match trimmed_line.parse::<PageRule>() {
                Ok(rule) => page_rules.push(rule),
                Err(e) => eprintln!("Failed to parse Page Rule: {}. Error: {}", trimmed_line, e),
            }
        }
    }

    Ok((page_rules, updates))
}
struct PageRule {
    low: String,
    high: String,
}

impl FromStr for PageRule {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (low, high) = s
            .split_once('|')
            .ok_or_else(|| anyhow!("Bad Page Rule: {s}"))?;
        Ok(PageRule { 
            low: low.trim().to_string(), 
            high: high.trim().to_string(),
        })
    }
}

