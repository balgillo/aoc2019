use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};


fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path: &String = &args[1];

    let f = File::open(file_path).expect("Unable to open file");
    let f = BufReader::new(f);

    let mut fli = f.lines();
    let wire1_moves = load_moves(&fli.next().expect("No wire 1 moves").expect("No wire 1 moves"));
    let wire2_moves = load_moves(&fli.next().expect("No wire 2 moves").expect("No wire 2 moves"));

    println!("{}", find_closest_crossing(&wire1_moves, &wire2_moves));
}


fn load_moves(moves_csv: &str) -> Vec<Move> {
    let mut ret = Vec::<Move>::new();
    for s in moves_csv.split(",") {
        ret.push(parse_move(&s));
    }
    ret
}


enum Direction {
    Up,
    Down,
    Left,
    Right
}


struct Move {
    direction: Direction,
    distance: i32
}


impl Move {

    fn apply(&self, pos: &WirePos) -> WirePos {
        match self.direction {
            Direction::Up =>    WirePos{x: pos.x, y: pos.y - self.distance},
            Direction::Down =>  WirePos{x: pos.x, y: pos.y + self.distance},
            Direction::Left =>  WirePos{x: pos.x - self.distance, y: pos.y},
            Direction::Right => WirePos{x: pos.x + self.distance, y: pos.y},
        }
    }

    fn to_segment(&self, pos: &WirePos) -> WireSegment {
        match self.direction {
            Direction::Up =>    WireSegment{x0: pos.x, y0: pos.y - self.distance, x1: pos.x, y1: pos.y},
            Direction::Down =>  WireSegment{x0: pos.x, y0: pos.y, x1: pos.x, y1: pos.y + self.distance},
            Direction::Left =>  WireSegment{x0: pos.x - self.distance, y0: pos.y, x1: pos.x, y1: pos.y},
            Direction::Right => WireSegment{x0: pos.x, y0: pos.y, x1: pos.x + self.distance, y1: pos.y},
        }
    }
}


struct WireSegment {
    x0: i32,
    y0: i32,
    x1: i32,
    y1: i32,
}

impl WireSegment {
    fn is_horizontal(&self) -> bool {
        self.y0 == self.y1
    }

    fn crossing_point(&self, other_wire_segment: &WireSegment) -> Option::<(i32, i32)> {
        match (self.is_horizontal(), other_wire_segment.is_horizontal()) {
            (true, true) => None,
            (false, false) => None,
            (true, false) =>
                if other_wire_segment.x0 >= self.x0 && other_wire_segment.x0 <= self.x1 && self.y0 >= other_wire_segment.y0 && self.y0 <= other_wire_segment.y1 {
                    Some((other_wire_segment.x0, self.y0))
                } else {
                    None
                },
            (false, true) =>
                if other_wire_segment.y0 >= self.y0 && other_wire_segment.y0 <= self.y1 && self.x0 >= other_wire_segment.x0 && self.x0 <= other_wire_segment.x1 {
                    Some((self.x0, other_wire_segment.y0))
                } else {
                    None
                },
        }
    }
}


struct WirePos {
    x: i32,
    y: i32,
}


fn parse_direction(c: char) -> Direction {
    match c {
        'U' => Direction::Up,
        'D' => Direction::Down,
        'L' => Direction::Left,
        'R' => Direction::Right,
        _ => panic!("Bad direction"),
    }
}


fn parse_move(m : &str) -> Move {
    Move{
        direction: parse_direction(m.chars().next().expect("Empty move")),
        distance: str::parse::<i32>(&m[1..]).expect("Bad step count")
    }
}


fn minimal(d1: Option<i32>, d2: Option<i32>) -> Option<i32> {
    match (d1, d2) {
        (None, None) => None,
        (None, Some(y)) => Some(y),
        (Some(x), None) => Some(x),
        (Some(0), Some(y)) => Some(y),
        (Some(x), Some(0)) => Some(x),
        (Some(x), Some(y)) => if x < y {Some(x)} else {Some(y)},
    }
}


fn manhattan_distance_from_origin(x: i32, y: i32) -> i32 {
    x.abs() + y.abs()
}


fn find_closest_crossing(wire1_moves: &Vec::<Move>, wire2_moves: &Vec::<Move>) -> i32 {
    let mut wire1_pos = WirePos{x: 0, y: 0};
    let mut best_crossing_distance: Option::<i32> = None;
    for wire1_move in wire1_moves {
        let wire1_segment = wire1_move.to_segment(&wire1_pos);
        let mut wire2_pos = WirePos{x: 0, y: 0};
        for wire2_move in wire2_moves {
            let wire2_segment = wire2_move.to_segment(&wire2_pos);
            match wire1_segment.crossing_point(&wire2_segment) {
                Some((cx, cy)) => {
                    best_crossing_distance = minimal(best_crossing_distance, Some(manhattan_distance_from_origin(cx, cy)));
                },
                None => (),
            }
            wire2_pos = wire2_move.apply(&wire2_pos);
        }
        wire1_pos = wire1_move.apply(&wire1_pos);
    }
    best_crossing_distance.expect("No crossing found!")
}
