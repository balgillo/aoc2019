use std::env;
use std::io::{stdin, stdout, Write};

fn main() {
    let l = env::args().skip(1).next().expect("Missing argument");
    let init_mem: Vec<i32> = l.split(",").map(|x| i32::from_str_radix(&x, 10).expect("Can't parse")).collect();
    let mut mem = init_mem.clone();
    run(&mut mem);
    println!("{:?}", mem);
}


enum ParamMode {
    Indirect,
    Direct
}


fn read(mem: &Vec<i32>, index: usize, mode: ParamMode) -> i32 {
    match mode {
        ParamMode::Indirect => mem[mem[index] as usize],
        ParamMode::Direct => mem[index],
    }
}


fn param_mode(code: i32) -> ParamMode {
    match code {
        0 => ParamMode::Indirect,
        1 => ParamMode::Direct,
        _ => panic!(format!("Bad param mode {}", code)),
    }
}


fn param_1_mode(cmd: i32) -> ParamMode {
    param_mode((cmd / 100) % 10)
}


fn param_2_mode(cmd: i32) -> ParamMode {
    param_mode((cmd / 1000) % 10)
}


fn run(mem: &mut Vec<i32>) {
    let mut ip: usize = 0;
    loop {
        let cmd = mem[ip];
        let opcode = cmd % 100;
        match opcode {
            1 => { /* add */
                let a: i32 = read(mem, ip + 1 as usize, param_1_mode(cmd));
                let b: i32 = read(mem, ip + 2 as usize, param_2_mode(cmd));
                let dest: usize = mem[ip + 3] as usize;
                mem[dest] = a + b;
                ip += 4;
            },
            2 => { /* mul */
                let a: i32 = read(mem, ip + 1 as usize, param_1_mode(cmd));
                let b: i32 = read(mem, ip + 2 as usize, param_2_mode(cmd));
                let dest: usize = mem[ip + 3] as usize;
                mem[dest] = a * b;
                ip += 4;
            },
            3 => { /* input */
                print!("Input: ");
                stdout().flush().expect("Can't flush");
                let mut input_string : String = String::new();
                stdin().read_line(&mut input_string).expect("No input");
                let input_value: i32 = i32::from_str_radix(input_string.trim(), 10).expect("Can't parse input");
                let dest: usize = mem[ip + 1] as usize;
                mem[dest] = input_value;
                ip += 2;
            },
            4 => { /* output */
                let a: i32 = read(mem, ip + 1 as usize, param_1_mode(cmd));
                println!("Output: {}", a);
                ip += 2;
            },
            5 => { /* jump if true */
                let a: i32 = read(mem, ip + 1 as usize, param_1_mode(cmd));
                let b: i32 = read(mem, ip + 2 as usize, param_2_mode(cmd));
                match a {
                    0 => ip += 3,
                    _ => ip = b as usize,
                }
            },
            6 => { /* jump if false */
                let a: i32 = read(mem, ip + 1 as usize, param_1_mode(cmd));
                let b: i32 = read(mem, ip + 2 as usize, param_2_mode(cmd));
                match a {
                    0 => ip = b as usize,
                    _ => ip += 3,
                }
            },
            7 => { /* less than */
                let a: i32 = read(mem, ip + 1 as usize, param_1_mode(cmd));
                let b: i32 = read(mem, ip + 2 as usize, param_2_mode(cmd));
                let dest: usize = mem[ip + 3] as usize;
                mem[dest] = if a < b {1} else {0};
                ip += 4;
            },
            8 => { /* equals */
                let a: i32 = read(mem, ip + 1 as usize, param_1_mode(cmd));
                let b: i32 = read(mem, ip + 2 as usize, param_2_mode(cmd));
                let dest: usize = mem[ip + 3] as usize;
                mem[dest] = if a == b {1} else {0};
                ip += 4;
            },
            99 => break, /* halt */
            _ => panic!("Bad opcode"),
        }
    }
}