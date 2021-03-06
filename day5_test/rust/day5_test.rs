use std::env;
use std::fs::File;
use std::io::{stdin, stdout, Write, BufRead, BufReader};
use std::collections::VecDeque;

fn main() {
    let file_path = env::args().skip(1).next().expect("Missing argument");
    let program = load_program(&file_path);

    let mut computer = IntCodeComputer::new(&program);
    loop {
        computer.run();
        match &computer.state {
            ComputerState::WaitingForInput => {
                print!("Input: ");
                stdout().flush().expect("Can't flush");
                let mut input_string = String::new();
                stdin().read_line(&mut input_string).expect("No input");
                let input_value = i32::from_str_radix(input_string.trim(), 10).expect("Can't parse input");
                computer.inputs.push_back(input_value)
            },
            ComputerState::Halted => {
                break
            },
            _ => panic!("Bad computer state")
        }
    }
    println!("Output: {:?}", computer.outputs);
    println!("Memory: {:?}", computer.mem);
}


fn load_program(file_path: &str) -> Vec<i32> {
    let f = File::open(file_path).expect("Unable to open file");
    let f_reader = BufReader::new(f);
    let line = f_reader.lines().next().expect("No program in file").expect("No program in file");
    let program: Vec<i32> = line.split(",").map(|x| i32::from_str_radix(&x, 10).expect("Can't parse")).collect();
    program
}


enum ComputerState {
    Ready,
    WaitingForInput,
    Halted,
}


enum ParamMode {
    Indirect,
    Direct,
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
                99 => { /* halt */
                    self.state = ComputerState::Halted;
                    break;
                },
                _ => panic!("Bad opcode"),
            }
        }
    }

}








