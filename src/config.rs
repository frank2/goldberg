#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Config {
    pub codeflow: bool,
    pub strings: bool,
    pub ints: bool,
}
impl Config {
    pub fn new(codeflow: bool, strings: bool, ints: bool) -> Self { Self { codeflow, strings, ints } }
}
