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
            Direction::Up =>    WirePos{x: pos.x, y: pos.y - self.distance, step_count: pos.step_count + self.distance},
            Direction::Down =>  WirePos{x: pos.x, y: pos.y + self.distance, step_count: pos.step_count + self.distance},
            Direction::Left =>  WirePos{x: pos.x - self.distance, y: pos.y, step_count: pos.step_count + self.distance},
            Direction::Right => WirePos{x: pos.x + self.distance, y: pos.y, step_count: pos.step_count + self.distance},
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
    step_count: i32,
}

impl WirePos {
    fn total_steps_to(&self, point_x: i32, point_y: i32) -> i32 {
        self.step_count + (point_x - self.x).abs() + (point_y - self.y).abs()
    }
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


fn minimal(d1: Option<i32>, d2: i32) -> Option<i32> {
    match (d1, d2) {
        (None, 0) => None,
        (None, y) => Some(y),
        (Some(0), 0) => None,
        (Some(0), y) => Some(y),
        (Some(x), 0) => Some(x),
        (Some(x), y) => if x < y {Some(x)} else {Some(y)},
    }
}


fn find_closest_crossing(wire1_moves: &Vec::<Move>, wire2_moves: &Vec::<Move>) -> i32 {
    let mut wire1_pos = WirePos{x: 0, y: 0, step_count: 0};
    let mut best_crossing_distance: Option::<i32> = None;
    for wire1_move in wire1_moves {
        let wire1_segment = wire1_move.to_segment(&wire1_pos);
        let mut wire2_pos = WirePos{x: 0, y: 0, step_count: 0};
        for wire2_move in wire2_moves {
            let wire2_segment = wire2_move.to_segment(&wire2_pos);
            match wire1_segment.crossing_point(&wire2_segment) {
                Some((cx, cy)) => {
                    let total_steps_to_crossing_point = wire1_pos.total_steps_to(cx, cy) + wire2_pos.total_steps_to(cx, cy);
                    best_crossing_distance = minimal(best_crossing_distance, total_steps_to_crossing_point);
                },
                None => (),
            }
            wire2_pos = wire2_move.apply(&wire2_pos);
        }
        wire1_pos = wire1_move.apply(&wire1_pos);
    }
    best_crossing_distance.expect("No crossing found!")
}
