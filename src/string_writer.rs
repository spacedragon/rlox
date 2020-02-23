use std::io::Write;

pub trait StringWriter {
    fn write_string(&mut self, s: &str);
}

impl StringWriter for String {
    fn write_string(&mut self, s: &str) {
        self.push_str(s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Output {
    Stdout,
    String(String),
}

impl StringWriter for Output {
    fn write_string(&mut self, str: &str) {
        match self {
            Output::String(s) => {
                s.push_str(str)
            },
            Output::Stdout =>
                std::io::stdout().lock()
                    .write_all(str.as_bytes()).expect("can't output str to stdout")
        }
    }
}