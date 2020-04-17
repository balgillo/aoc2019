use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path: &String = &args[1];

    let f = File::open(file_path).expect("Unable to open file");
    let f = BufReader::new(f);

    let mut masses = Vec::new();
    for line in f.lines() {
        let mass: i32 = line.expect("Unable to read line").trim().parse::<i32>().expect("Can't parse");
        masses.push(mass);
    }
    let total_fuel = masses.iter().fold(0, |sum, x| sum + module_fuel(*x));
    
    println!("{}", total_fuel);
}


fn module_fuel(x: i32) -> i32 {
    let f = (x / 3) - 2;
    if f <= 0 {
        0
    } else {
        f + module_fuel(f)
    }
}
