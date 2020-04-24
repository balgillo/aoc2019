use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::VecDeque;
use num_bigint::{BigInt, Sign};
use num_traits::{Zero, One};
use num::ToPrimitive;
#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref B_0: BigInt = BigInt::zero();
    static ref B_1: BigInt = BigInt::one();
    static ref B_10: BigInt = BigInt::from(10);
    static ref B_100: BigInt = BigInt::from(100);
    static ref B_1000: BigInt = BigInt::from(1000);
    static ref B_10000: BigInt = BigInt::from(10000);
}

fn main() {
    let file_path = env::args().skip(1).next().expect("Missing argument");
    let program = load_program(&file_path);

    let mut computer = IntCodeComputer::new(&program);
    computer.inputs.push_back(BigInt::one());
    computer.run();
    print_bigints(&computer.outputs);
}


fn load_program(file_path: &str) -> Vec<BigInt> {
    let f = File::open(file_path).expect("Unable to open file");
    let f_reader = BufReader::new(f);
    let line = f_reader.lines().next().expect("No program in file").expect("No program in file");
    let program: Vec<BigInt> = line.split(",").map(|x| BigInt::parse_bytes(x.as_bytes(), 10).expect("Can't parse")).collect();
    program
}


fn print_bigints(bigints: &Vec<BigInt>) {
    print!("[");
    let mut first = true;
    for a in bigints {
        if first {
            first = false;
        } else {
            print!(", ");
        }
        print!("{}", a.to_str_radix(10));
    }
    println!("]");

}

#[derive(Debug)]
enum ComputerState {
    Ready,
    WaitingForInput,
    Halted,
}

#[derive(Debug)]
enum ParamMode {
    Indirect,
    Direct,
    Relative
}


impl ParamMode {
    fn from(code: i32) -> ParamMode {
        match code {
            0 => ParamMode::Indirect,
            1 => ParamMode::Direct,
            2 => ParamMode::Relative,
            _ => panic!(format!("Bad param mode {}", code)),
        }
    }
}


struct IntCodeComputer {
    mem: Vec<BigInt>,
    ip: usize,
    relative_base: usize,
    state: ComputerState,
    inputs: VecDeque<BigInt>,
    outputs: Vec<BigInt>,
}


impl IntCodeComputer {
    fn new(program : &Vec<BigInt>) -> IntCodeComputer {
        IntCodeComputer{
            mem: program.clone(),
            ip: 0,
            relative_base: 0,
            state: ComputerState::Ready,
            inputs: VecDeque::new(),
            outputs: Vec::new()
        }
    }

    fn peek(&self, address: usize) -> BigInt {
        if address < self.mem.len() {
            self.mem[address].clone()
        } else {
            BigInt::zero()
        }
    }

    fn peek_as_usize(&self, address: usize) -> usize {
        match self.peek(address).to_usize() {
            Some(ret) => ret,
            None => panic!(format!("Memory entry at address {} out of range for usize: {}", address, self.peek(address)))
        }
    }

    fn peek_div_mod(&self, address: usize, divisor: &BigInt, modulus: &BigInt) -> i32 {
        let val_bigint = (self.peek(address) / divisor) % modulus;
        match val_bigint.to_i32() {
            Some(ret) => ret,
            None => panic!(format!("Cant calculate (peek({})/{})%{}", address, divisor, modulus))
        }
    }

    fn address_from_param(&self, param_address: usize, mode: ParamMode) -> usize {
        match mode {
            ParamMode::Indirect => self.peek_as_usize(param_address),
            ParamMode::Direct => param_address,
            ParamMode::Relative => (self.peek(param_address) + self.relative_base).to_usize().expect("Relative address out of range for usize")
        }
    }

    fn read(&self, param_address: usize, mode: ParamMode) -> BigInt {
        self.peek(self.address_from_param(param_address, mode))
    }

    fn read_param_1(&self) -> BigInt {
        let param_mode_val = self.peek_div_mod(self.ip, &B_100, &B_10);
        self.read(self.ip + 1, ParamMode::from(param_mode_val))
    }

    fn read_param_2(&self) -> BigInt {
        let param_mode_val = self.peek_div_mod(self.ip, &B_1000, &B_10);
        self.read(self.ip + 2, ParamMode::from(param_mode_val))
    }

    fn address_from_param_1(&self) -> usize {
        let param_mode_val = self.peek_div_mod(self.ip, &B_100, &B_10);
        self.address_from_param(self.ip + 1, ParamMode::from(param_mode_val))
    }

    fn address_from_param_3(&self) -> usize {
        let param_mode_val = self.peek_div_mod(self.ip, &B_10000, &B_10);
        self.address_from_param(self.ip + 3, ParamMode::from(param_mode_val))
    }

    fn poke(&mut self, address: usize, value: BigInt) {
        if address < self.mem.len() {
            self.mem[address] = value.clone();
        } else {
            while address > self.mem.len() {
                self.mem.push(BigInt::zero());
            }
            self.mem.push(value.clone());
        }
    }

    fn run(&mut self) {
        self.state = ComputerState::Ready;
        self.outputs.clear();
        loop {
            match self.peek_div_mod(self.ip, &B_1, &B_100) {
                1 => { /* add */
                    let a = self.read_param_1();
                    let b = self.read_param_2();
                    let dest = self.address_from_param_3();
                    self.poke(dest, a + b);
                    self.ip += 4;
                },
                2 => { /* mul */
                    let a = self.read_param_1();
                    let b = self.read_param_2();
                    let dest = self.address_from_param_3();
                    self.poke(dest, a * b);
                    self.ip += 4;
                },
                3 => { /* input */
                    match self.inputs.pop_front() {
                        None => {
                            self.state = ComputerState::WaitingForInput;
                            break;
                        },
                        Some(input_value) => {
                            let dest = self.address_from_param_1();
                            self.poke(dest, input_value);
                            self.ip += 2;
                        }
                    }
                },
                4 => { /* output */
                    let a = self.read_param_1();
                    self.outputs.push(a);
                    self.ip += 2;
                },
                5 => { /* jump if true */
                    let a = self.read_param_1();
                    let b = self.read_param_2();
                    match a.sign() {
                        Sign::NoSign => self.ip += 3,
                        _ => self.ip = b.to_usize().expect("Jump address does not fit in usize"),
                    }
                },
                6 => { /* jump if false */
                    let a = self.read_param_1();
                    let b = self.read_param_2();
                    match a.sign() {
                        Sign::NoSign => self.ip = b.to_usize().expect("Jump address does not fit in usize"),
                        _ => self.ip += 3,
                    }
                },
                7 => { /* less than */
                    let a = self.read_param_1();
                    let b = self.read_param_2();
                    let dest = self.address_from_param_3();
                    self.poke(dest, if a < b {BigInt::one()} else {BigInt::zero()});
                    self.ip += 4;
                },
                8 => { /* equals */
                    let a = self.read_param_1();
                    let b = self.read_param_2();
                    let dest = self.address_from_param_3();
                    self.poke(dest, if a == b {BigInt::one()} else {BigInt::zero()});
                    self.ip += 4;
                },
                9 => { /* add to relative base */
                    let a = self.read_param_1();
                    match a.sign() {
                        Sign::Plus => self.relative_base += a.to_usize().expect("Relative base addend out of range"),
                        Sign::Minus => self.relative_base -= (-a).to_usize().expect("Relative base addend out of range"),
                        Sign::NoSign => ()
                    }
                    self.ip += 2;
                },
                99 => { /* halt */
                    self.state = ComputerState::Halted;
                    break;
                },
                _ => panic!("Bad opcode"),
            }
        }
    }
}