use std::env;


fn main() {
    let l = env::args().skip(1).next().expect("Missing argument");
    let init_mem: Vec<i32> = l.split(",").map(|x| i32::from_str_radix(&x, 10).expect("Can't parse")).collect();
    let mut mem = init_mem.clone();
    run(&mut mem);
    println!("{}", mem[0]);
}


fn run(mem: &mut Vec<i32>) {
    let mut ip: usize = 0;
    loop {
        let cmd = mem[ip];
        match cmd {
            1 => {
                let a: i32 = mem[mem[ip + 1] as usize];
                let b: i32 = mem[mem[ip + 2] as usize];
                let dest: usize = mem[ip + 3] as usize;
                mem[dest] = a + b;
                ip += 4;
            },
            2 => {
                let a: i32 = mem[mem[ip + 1] as usize];
                let b: i32 = mem[mem[ip + 2] as usize];
                let dest: usize = mem[ip + 3] as usize;
                mem[dest] = a * b;
                ip += 4;
            },
            99 => break,
            _ => panic!("Bad opcode"),
        }
    }
}