

pub trait StringWriter {
    fn write_string(&mut self, s: &str);
}

impl StringWriter for String {
    fn write_string(&mut self, s: &str) {
        self.push_str(s)
    }
}