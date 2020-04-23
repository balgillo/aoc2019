use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::VecDeque;

fn main() {
    let file_path = env::args().skip(1).next().expect("Missing argument");
    let program = load_program(&file_path);

    let mut max_amp_output = 0;
    for amp_phases in iterate_permutations(&[5, 6, 7, 8, 9]) {
        let mut amp_output = 0;
        let mut computers = Vec::new();
        for i in 0..5 {
            let mut computer = IntCodeComputer::new(&program);
            computer.inputs.push_back(amp_phases[i]);
            computer.inputs.push_back(amp_output);
            computer.run();
            amp_output = computer.outputs[0];
            computers.push(computer);
        }
        // now keep running computers until last computer halts
        while !computers.last().unwrap().is_halted() {
            for computer in computers.iter_mut() {
                if computer.is_waiting_for_input() {
                    computer.inputs.push_back(amp_output);
                    computer.run();
                }
                amp_output = computer.outputs[0];
            }
        }
        if amp_output > max_amp_output {
            max_amp_output = amp_output;
        }
    }
    println!("{}", max_amp_output);
}


fn load_program(file_path: &str) -> Vec<i32> {
    let f = File::open(file_path).expect("Unable to open file");
    let f_reader = BufReader::new(f);
    let line = f_reader.lines().next().expect("No program in file").expect("No program in file");
    let program: Vec<i32> = line.split(",").map(|x| i32::from_str_radix(&x, 10).expect("Can't parse")).collect();
    program
}


fn iterate_permutations(items: &[i32]) -> PermutationGenerator{
    PermutationGenerator{items: items.to_vec(), lehmer_code: vec![0; items.len()]}
}


struct PermutationGenerator {
    items: Vec<i32>,
    /* self.lehmer_code is a factoradic number ([0] = ones digit, [1] = twos digit, [2] = threes digit
      etc.) that can be turned into a permutation of self.items via the "Lehmer code".
      See https://en.wikipedia.org/wiki/Factorial_number_system#Permutations */
    lehmer_code: Vec<usize>,
}


impl Iterator for PermutationGenerator {
    type Item = Vec<i32>;

    fn next(&mut self) -> Option<Vec<i32>> {
        if self.lehmer_code.is_empty() {
            None
        } else {
            let mut remaining_items = self.items.clone();
            let mut ret = Vec::new();

            // Generate permutation by Lehmer code
            for i in self.lehmer_code.iter().rev() {
                ret.push(remaining_items.remove(*i));
            }

            // Increment factoradic number
            let mut overflow = true;
            for i in 0..self.lehmer_code.len() {
                if self.lehmer_code[i] < i {
                    self.lehmer_code[i] += 1;
                    overflow = false;
                    break;
                } else {
                    self.lehmer_code[i] = 0;
                }
            }
            if overflow {
                self.lehmer_code.clear();
            }
            Some(ret)
        }
    }
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
    Direct
}


impl ParamMode {
    fn from_int(code: i32) -> ParamMode {
        match code {
            0 => ParamMode::Indirect,
            1 => ParamMode::Direct,
            _ => panic!(format!("Bad param mode {}", code)),
        }
    }
}


struct IntCodeComputer {
    mem: Vec<i32>,
    ip: usize,
    state: ComputerState,
    inputs: VecDeque<i32>,
    outputs: Vec<i32>,
}


impl IntCodeComputer {
    fn new(program : &Vec<i32>) -> IntCodeComputer {
        IntCodeComputer{
            mem: program.clone(), 
            ip: 0, 
            state: ComputerState::Ready,
            inputs: VecDeque::new(),
            outputs: Vec::new()
        }
    }

    fn is_halted(&self) -> bool {
        match self.state {
            ComputerState::Halted => true,
            ComputerState::WaitingForInput | ComputerState::Ready => false
        }
    }

    fn is_waiting_for_input(&self) -> bool {
        match self.state {
            ComputerState::WaitingForInput => true,
            ComputerState::Halted | ComputerState::Ready => false

        }
    }

    fn read(&self, address: usize, mode: ParamMode) -> i32 {
        match mode {
            ParamMode::Indirect => self.mem[self.mem[address] as usize],
            ParamMode::Direct => self.mem[address],
        }
    }

    fn read_param_1(&self) -> i32 {
        self.read(self.ip + 1, ParamMode::from_int((self.mem[self.ip] / 100) % 10))
    }

    fn read_param_2(&self) -> i32 {
        self.read(self.ip + 2, ParamMode::from_int((self.mem[self.ip] / 1000) % 10))
    }

    fn run(&mut self) {
        self.state = ComputerState::Ready;
        self.outputs.clear();
        loop {
            let cmd = self.mem[self.ip];
            match cmd % 100 {
                1 => { /* add */
                    let a: i32 = self.read_param_1();
                    let b: i32 = self.read_param_2();
                    let dest: usize = self.mem[self.ip + 3] as usize;
                    self.mem[dest] = a + b;
                    self.ip += 4;
                },
                2 => { /* mul */
                    let a: i32 = self.read_param_1();
                    let b: i32 = self.read_param_2();
                    let dest: usize = self.mem[self.ip + 3] as usize;
                    self.mem[dest] = a * b;
                    self.ip += 4;
                },
                3 => { /* input */
                    match self.inputs.pop_front() {
                        None => {
                            self.state = ComputerState::WaitingForInput;
                            break;
                        },
                        Some(input_value) => {
                            let dest = self.mem[self.ip + 1] as usize;
                            self.mem[dest] = input_value;
                            self.ip += 2;
                        }
                    }
                },
                4 => { /* output */
                    let a: i32 = self.read_param_1();
                    self.outputs.push(a);
                    self.ip += 2;
                },
                5 => { /* jump if true */
                    let a: i32 = self.read_param_1();
                    let b: i32 = self.read_param_2();
                    match a {
                        0 => self.ip += 3,
                        _ => self.ip = b as usize,
                    }
                },
                6 => { /* jump if false */
                    let a: i32 = self.read_param_1();
                    let b: i32 = self.read_param_2();
                    match a {
                        0 => self.ip = b as usize,
                        _ => self.ip += 3,
                    }
                },
                7 => { /* less than */
                    let a: i32 = self.read_param_1();
                    let b: i32 = self.read_param_2();
                    let dest = self.mem[self.ip + 3] as usize;
                    self.mem[dest] = if a < b {1} else {0};
                    self.ip += 4;
                },
                8 => { /* equals */
                    let a: i32 = self.read_param_1();
                    let b: i32 = self.read_param_2();
                    let dest: usize = self.mem[self.ip + 3] as usize;
                    self.mem[dest] = if a == b {1} else {0};
                    self.ip += 4;
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