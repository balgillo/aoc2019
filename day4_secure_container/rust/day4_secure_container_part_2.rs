use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let min_inclusive: i32 = args[1].parse::<i32>().unwrap();
    let max_inclusive: i32 = args[2].parse::<i32>().unwrap();
    println!("{}", count_codes(min_inclusive, max_inclusive));
}

fn count_codes(min_inclusive: i32, max_inclusive: i32) -> i32 {
    let mut ret = 0;
    let mut n = min_inclusive;
    while n <= max_inclusive {
        ret += count_if_valid(n);
        n += 1;
    }
    ret
}

fn count_if_valid(x: i32) -> i32 {
    let x_string: String = x.to_string();
    let mut x_chars = x_string.chars();
    match x_chars.next() {
        None => 0,
        Some(first_char) => {
            let mut preceding_char = first_char;
            let mut run_length = 1;
            let mut found_run2 = false;
            let mut non_decreasing = true;
            loop {
                match x_chars.next() {
                    None => break,
                    Some(c) => {
                        if c < preceding_char {
                            non_decreasing = false;
                            break;
                        } else if c == preceding_char {
                            run_length += 1;
                        } else {
                            found_run2 = found_run2 || run_length == 2;
                            run_length = 1;
                        }
                        preceding_char = c;
                    }
                }
            }
            found_run2 = found_run2 || run_length == 2;
            match non_decreasing && found_run2 {
                false => 0,
                true => 1,
            }
        },
    }
}
