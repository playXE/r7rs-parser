pub struct ScanBuffer {
    buffer: Vec<char>,
    index: usize
}

impl ScanBuffer {
    pub fn index(&self) -> usize {
        self.index
    }

    pub fn new(cap: usize) -> Self {
        Self {
            buffer: vec![0 as char; cap],
            index: 0
        }
    }

    pub fn reset(&mut self) {
        if self.index > 0 {
            self.buffer[0] = self.buffer[self.index - 1];
            self.index = 1;
        }
    }

    pub fn append(&mut self, c: char) {
        if self.index < self.buffer.len() {
            self.buffer[self.index] = c;
        } else {
            self.buffer.push(c);
        }

        self.index += 1;
    }

    pub fn string_value(&self) -> String {
        if self.index > 0 {
            self.buffer[0..self.index - 1].iter().collect()
        } else {
            "".to_string()
        }
    }

    pub fn string_starting_at(&self, start: usize) -> String {
        if start == 0 {
            return self.string_value()
        }
        if start < self.index {
            self.buffer[start..self.index - 1].iter().collect()
        } else {
            "".to_string()
        }
    }
}