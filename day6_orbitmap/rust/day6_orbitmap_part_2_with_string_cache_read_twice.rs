use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader, Seek, SeekFrom};
use std::collections::HashMap;


struct StringCacher {
    cache: Vec<String>
}

impl StringCacher {
    fn new() -> StringCacher{
        StringCacher{cache: Vec::new()}
    }

    fn cache(&mut self, s: &str) {
        match self.cache.iter().find(|&cached| cached == s) {
            None => self.cache.push(s.to_string()),
            Some(_) => (),
        }
    }

    fn get(&self, s: &str) -> &str {
        match self.cache.iter().find(|&cached| cached == s) {
            None => panic!(format!("Uncached string {}", s)),
            Some(cached) => &cached,
        }
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path: &String = &args[1];

    let f = File::open(file_path).expect("Unable to open file");
    let mut f_reader = BufReader::new(f);

    let planet_name_cache = precache_planet_names(&mut f_reader);
    f_reader.seek(SeekFrom::Start(0)).expect("Failed to seek!");

    let graph: HashMap<&str, &str> = load_graph(&mut f_reader, &planet_name_cache);

    let you_path = get_path_to_galactic_centre("YOU", &graph, &planet_name_cache);
    let santa_path = get_path_to_galactic_centre("SAN", &graph, &planet_name_cache);
    let transfers = count_transfers(&you_path, &santa_path);

    println!("{}", transfers);
}


fn precache_planet_names(f_reader: &mut dyn BufRead) -> StringCacher {
    let mut cacher = StringCacher::new();
    for line in f_reader.lines() {
        let line = line.expect("Unable to read line");
        let tokens:Vec<&str> = line.split(")").collect();
        cacher.cache(&tokens[1]);
        cacher.cache(&tokens[0]);
    }
    cacher
}


fn load_graph<'a>(f_reader: &mut dyn BufRead, string_cache: &'a StringCacher) -> HashMap<&'a str, &'a str> {
    let mut graph = HashMap::new();
    for line in f_reader.lines() {
        let line = line.expect("Unable to read line");
        let tokens:Vec<&str> = line.split(")").collect();
        let satellite = string_cache.get(&tokens[1]);
        let centre = string_cache.get(&tokens[0]);
        graph.insert(satellite, centre);
    }
    graph
}


fn get_path_to_galactic_centre<'a, 'b>(satellite: &'b str, graph: &HashMap::<&'a str, &'a str>, string_cache: &'a StringCacher) -> Vec<&'a str> {
    let mut ret = Vec::new();
    let mut n: &str = satellite;
    loop {
        match graph.get(n) {
            Some(m) => {
                n = &m;
                ret.push(string_cache.get(n));
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
