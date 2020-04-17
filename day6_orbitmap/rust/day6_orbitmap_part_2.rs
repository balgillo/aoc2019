use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::{HashMap, HashSet};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path: &String = &args[1];

    let f = File::open(file_path).expect("Unable to open file");
    let f = BufReader::new(f);

    let mut planet_name_cache = HashSet<Rc<String>>::new();
    let mut graph = HashMap::new();
    for line in f.lines() {
        let line = line.expect("Unable to read line");
        let tokens:Vec<&str> = line.split(")").collect();
        let satellite = String::from(tokens[1]);
        let centre = String::from(tokens[0]);
        graph.insert(satellite, centre);
    }

    let you_path = get_path_to_galactic_centre("YOU", &graph);
    let santa_path = get_path_to_galactic_centre("SAN", &graph);
    let transfers = count_transfers(&you_path, &santa_path);

    println!("{}", transfers);
}


fn get_path_to_galactic_centre<'a>(satellite: &'a str, graph: &'a HashMap::<String, String>) -> Vec<&'a str> {
    let mut ret = Vec::new();
    let mut n: &str = satellite;
    loop {
        match graph.get(n) {
            Some(m) => {
                n = &m;
                ret.push(n);
            },
            None => return ret,
        }
    }
}


fn count_transfers(you_path: &Vec<&str>, santa_path: &Vec<&str>) -> usize {
    for (you_distance, &p) in you_path.iter().enumerate() {
        match santa_path.iter().position(|&s| s == p) {
            Some(santa_distance) => return you_distance + santa_distance,
            None => continue,
        }
    }
    panic!("No path");
}
