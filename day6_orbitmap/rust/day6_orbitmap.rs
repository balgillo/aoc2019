use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::HashMap;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path: &String = &args[1];

    let f = File::open(file_path).expect("Unable to open file");
    let f = BufReader::new(f);

    let mut graph = HashMap::new();
    for line in f.lines() {
        let line = line.expect("Unable to read line");
        let tokens:Vec<&str> = line.split(")").collect();
        let centre = tokens[0].to_string();
        let satellite = tokens[1].to_string();
        graph.insert(satellite, centre);
    }
    let mut total_orbits = 0;
    for satellite in graph.keys() {
        total_orbits += count_orbits(&satellite, &graph);
    }

    println!("{}", total_orbits);
}

fn count_orbits(satellite: &str, graph: &HashMap::<String, String>) -> i32 {
    let mut p = satellite;
    let mut total = 0;
    loop {
        match graph.get(p) {
            Some(centre) => {
                p = centre;
                total += 1;
            },
            None => return total,
        }
    }
}