use crate::{op::OpCode, value::Value};

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub values: Vec<Value>,
    pub line_nums: Vec<u32>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            line_nums: vec![],
            values: vec![],
        }
    }

    pub fn write(&mut self, op: OpCode, line_num: u32) {
        self.code.push(op);
        self.line_nums.push(line_num);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }

    pub fn read_constant(&self, index: usize) -> Value {
        return self.values[index]
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        for (i, (op, line)) in self.code.iter().zip(self.line_nums.iter()).enumerate() {
            println!("{:04} {} {:?}", i, line, op);
        }
    }
}
