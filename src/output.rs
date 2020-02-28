use std::io::{Write, Stdout};

pub trait Output {
    fn write_string(&mut self, s: String);
}

impl Output for String {
    fn write_string(&mut self, s: String) {
        self.push_str(s.as_str())
    }
}

impl Output for Stdout {
    fn write_string(&mut self, s: String) {
        self.lock().write_all(s.as_bytes()).expect("can't write to output");
    }
}

