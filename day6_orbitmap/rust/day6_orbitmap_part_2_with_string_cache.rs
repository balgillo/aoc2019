use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::HashMap;


struct Graph {
    // see https://www.reddit.com/r/rust/comments/665vda/how_do_you_structure_a_project_in_rust_without/
    nodes: Vec<String>,
    edges: HashMap<usize, usize>,
}


impl Graph {
    fn new() -> Graph {
        Graph{nodes: Vec::new(), edges: HashMap::new()}
    }

    fn add(&mut self, orbiter: &str, orbited: &str) {
        let mut orbiter_index = None;
        let mut orbited_index = None;
        for (i, s) in self.nodes.iter().enumerate() {
            if s == orbiter {
                orbiter_index = Some(i);
            } else if s == orbited {
                orbited_index = Some(i);
            }
        }
        match orbiter_index {
            None => {
                orbiter_index = Some(self.nodes.len());
                self.nodes.push(orbiter.to_string());
            }
            _ => ()
        }
        match orbited_index {
            None => {
                orbited_index = Some(self.nodes.len());
                self.nodes.push(orbited.to_string());
            }
            _ => ()
        }
        match (orbiter_index, orbited_index) {
            (Some(s), Some(c)) => {
                self.edges.insert(s, c);
            },
            _ => (),
        }
    }

    fn get_orbited(&self, orbiter: &str) -> Option<&str> {
        match self.nodes.iter().position(|cached| cached == orbiter) {
            None => None,
            Some(o) => {
                match self.edges.get(&o) {
                    None => None,
                    Some(i) => Some(&self.nodes[*i]),
                }
            }
        }
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path: &String = &args[1];

    let f = File::open(file_path).expect("Unable to open file");
    let mut f_reader = BufReader::new(f);

    let graph = load_graph(&mut f_reader);

    let you_path = get_path_to_galactic_centre("YOU", &graph);
    let santa_path = get_path_to_galactic_centre("SAN", &graph);
    let transfers = count_transfers(&you_path, &santa_path);

    println!("{}", transfers);
}


fn load_graph(f_reader: &mut dyn BufRead) -> Graph {
    let mut ret = Graph::new();
    for line in f_reader.lines() {
        let line = line.expect("Unable to read line");
        let tokens:Vec<&str> = line.split(")").collect();
        let satellite = &tokens[1];
        let centre = &tokens[0];
        ret.add(satellite, centre);
    }
    ret
}


fn get_path_to_galactic_centre<'a, 'b>(satellite: &'b str, graph: &'a Graph) -> Vec<&'a str> {
    let mut ret = Vec::new();
    let mut n: &str = satellite;
    loop {
        match graph.get_orbited(n) {
            Some(m) => {
                n = &m;
                ret.push(m);
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
