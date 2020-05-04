use std::clone::Clone;
use std::cmp::{Ordering, Ord};
use std::env;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::ops::{Sub, Neg};
use std::collections::VecDeque;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let monitoring_station = load_map_and_find_monitoring_station(file_path);
    println!("Best monitoring station is at {}, from where {} asteroids are visible",
        monitoring_station.position(), monitoring_station.count_visible_asteroids());
    monitoring_station.blast_asteroids();
}


#[derive(Clone, Copy)]
struct AsteroidPosition {
    x: i32,
    y: i32,
}


impl AsteroidPosition {
    fn blast(&self, order: i32) {
        println!("#{} blasted {}", order, self);
    }
}


impl fmt::Display for AsteroidPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}


#[derive(Clone, Copy, PartialEq, Eq)]
struct PositionOffset {
    dx: i32,
    dy: i32,
}


impl PositionOffset {
    fn is_zero(&self) -> bool {
        self.dx == 0 && self.dy == 0
    }

    fn quadrant(&self) -> i32 {
        if self.dx >= 0 {
            if self.dy <= 0 {0}
            else {1}
        } else {
            if self.dy > 0 {2}
            else {3}
        }
    }

    fn cmp_angle(&self, other: &PositionOffset) -> Ordering {
        let q1 = self.quadrant();
        let q2 = other.quadrant();
        if q1 < q2 {Ordering::Less}
        else if q1 > q2 {Ordering::Greater}
        else if self.is_zero() {
            if other.is_zero() {Ordering::Equal}
            else {Ordering::Less}
        } else {
            let slope_difference = other.dx * self.dy - other.dy * self.dx;
            if slope_difference < 0 {Ordering::Less}
            else if slope_difference > 0 {Ordering::Greater}
            else {Ordering::Equal}
        }
    }

    fn cmp_length(&self, other: &PositionOffset) -> Ordering {
        let my_length = self.dx.abs() + self.dy.abs();
        let other_length = other.dx.abs() + other.dy.abs();
        if my_length < other_length {Ordering::Less}
        else if my_length > other_length {Ordering::Greater}
        else {Ordering::Equal}
    }
}


impl Ord for PositionOffset {
    // Compare angles clockwise from top.  If same angle, compare length.
    fn cmp(&self, other: &Self) -> Ordering {
        match self.cmp_angle(&other) {
            Ordering::Less => {Ordering::Less}
            Ordering::Greater => {Ordering::Greater}
            Ordering::Equal => {self.cmp_length(&other)}
        }
    }
}


impl PartialOrd for PositionOffset {
    // Compare angles clockwise from top.  If same angle, compare length.
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}


impl Sub<AsteroidPosition> for AsteroidPosition {
    type Output = PositionOffset;

    fn sub(self, other: AsteroidPosition) -> PositionOffset {
        PositionOffset{dx: self.x - other.x, dy: self.y - other.y}
    }
}


impl Neg for PositionOffset {
    type Output = PositionOffset;

    fn neg(self) -> PositionOffset {
        PositionOffset{dx: -self.dx, dy: -self.dy}
    }
}


#[derive(Clone, Copy, PartialEq, Eq)]
struct AsteroidReference {
    offset: PositionOffset,
    index: usize,
}


impl AsteroidReference {
    fn new(offset: PositionOffset, index: usize) -> AsteroidReference {
        AsteroidReference{offset: offset, index: index}
    }
}


impl Ord for AsteroidReference {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.offset.cmp(&other.offset) {
            Ordering::Less => {Ordering::Less}
            Ordering::Greater => {Ordering::Greater}
            Ordering::Equal => {self.index.cmp(&other.index)}
        }
    }
}


impl PartialOrd for AsteroidReference {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}


struct AsteroidMapBuilder {
    asteroid_list: Vec<AsteroidPosition>,
    references: Vec<Vec<AsteroidReference>>,
}


impl AsteroidMapBuilder {
    fn new() -> AsteroidMapBuilder {
        AsteroidMapBuilder{asteroid_list: Vec::new(), references: Vec::new()}
    }

    fn add_asteroid(&mut self, x: i32, y: i32) {
        let new_asteroid_index = self.asteroid_list.len();
        let new_asteroid_position = AsteroidPosition{x: x, y: y};
        self.asteroid_list.push(new_asteroid_position);
        self.references.push(Vec::new());
        for old_asteroid_index in 0..new_asteroid_index {
            let d = new_asteroid_position - self.asteroid_list[old_asteroid_index];
            self.references[old_asteroid_index].push(AsteroidReference::new(d, new_asteroid_index));
            self.references[new_asteroid_index].push(AsteroidReference::new(-d, old_asteroid_index));
        }
    }

    fn count_distinct_directions(&self, index: usize) -> i32 {
        count_distinct_directions(&self.references[index])
    }

    fn build_monitoring_station(self) -> MonitoringStation {
        let mut monitoring_station_index = 0;
        let mut visible_from_monitoring_station = self.count_distinct_directions(monitoring_station_index);
        for i in 1..self.asteroid_list.len() {
            let m = self.count_distinct_directions(i);
            if m > visible_from_monitoring_station {
                monitoring_station_index = i;
                visible_from_monitoring_station = m;
            }
        }

        let mut asteroids_from_monitoring_station: Vec<AsteroidReference> = self.references[monitoring_station_index]
            .iter().cloned().filter(|x| !x.offset.is_zero()).collect();
        asteroids_from_monitoring_station.shrink_to_fit();

        MonitoringStation::new(self.asteroid_list, monitoring_station_index, asteroids_from_monitoring_station)
    }
}


struct MonitoringStation {
    asteroid_list: Vec<AsteroidPosition>,
    index: usize,
    other_asteroids: Vec<AsteroidReference>,
}


impl MonitoringStation {
    fn new(asteroid_list: Vec<AsteroidPosition>, index: usize, mut other_asteroids_unsorted: Vec<AsteroidReference>) -> MonitoringStation {
        // sort targets in clock order, then by distance
        other_asteroids_unsorted.sort();
        let other_asteroids_sorted = other_asteroids_unsorted;
        MonitoringStation{asteroid_list, index, other_asteroids: other_asteroids_sorted}
    }

    fn position(&self) -> AsteroidPosition {
        self.asteroid_list[self.index]
    }

    fn count_visible_asteroids(&self) -> i32 {
        count_distinct_directions(&self.other_asteroids)
    }

    fn blast_asteroids(&self) {
        let mut targets = VecDeque::new();
        for t in &self.other_asteroids {
            targets.push_back(t.clone());
        }

        let mut occluded_targets = VecDeque::new();
        let mut destroy_order = 1;
        let mut last_direction;
        loop {
            match targets.pop_front() {
                None => break,
                Some(t) => {
                    self.asteroid_list[t.index].blast(destroy_order);
                    destroy_order += 1;
                    last_direction = t.offset;
                }
            }
            while !targets.is_empty() {
                match targets.pop_front() {
                    None => return,
                    Some(t) => {
                        match t.offset.cmp_angle(&last_direction) {
                            Ordering::Equal => {
                                occluded_targets.push_back(t);
                            }
                            _ => {
                                self.asteroid_list[t.index].blast(destroy_order);
                                destroy_order += 1;
                                last_direction = t.offset;
                            }
                        }
                    }
                }
            }
            // targets.extend(occluded_targets.drain(..));
            loop {
                match occluded_targets.pop_front() {
                    Some(t) => targets.push_back(t),
                    None => break
                }
            }
        }
    }
}


fn count_distinct_directions(references: &Vec<AsteroidReference>) -> i32 {
    let mut ret = 0;
    let mut distinct_directions: Vec<PositionOffset> = Vec::new();
    for r in references {
        let mut already_counted = false;
        for d in &distinct_directions {
            if !r.offset.is_zero() {
                match d.cmp_angle(&r.offset) {
                    Ordering::Equal => {
                        already_counted = true;
                        break;
                    }
                    _ => ()
                }
            }
        }
        if !already_counted {
            distinct_directions.push(r.offset);
            ret += 1;
        }
    }
    ret
}


fn load_map_and_find_monitoring_station(file_path: &str) -> MonitoringStation {
    let mut f = File::open(file_path).expect("Unable to open file");
    let mut buf: [u8; 1] = [0];
    let (mut x, mut y) = (0, 0);
    let mut builder = AsteroidMapBuilder::new();

    loop {
        match f.read(&mut buf) {
            Ok(1) => {
                match buf[0] {
                    b'#' => {
                        builder.add_asteroid(x, y);
                        x = x + 1;
                    }
                    b'.' => {
                        x = x + 1;
                    }
                    b'\n' => {
                        x = 0;
                        y = y + 1;
                    }
                    _ => ()
                }
            }
            Ok(0) => {
                break;
            },
            Ok(_) | Err(_) => {
                panic!("File read failed");
            }
        }
    }
    builder.build_monitoring_station()
}
