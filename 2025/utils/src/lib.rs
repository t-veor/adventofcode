pub mod cycle_detection;
pub mod discrete_math;
pub mod disjoint_set;
pub mod grid;
pub mod parse;
pub mod pathfinding;

pub use glam;

#[macro_export]
macro_rules! read_input_file {
    () => {
        $crate::read_input_file!("input.txt")
    };
    ($fname:expr) => {{
        let args: Vec<_> = std::env::args().collect();
        let filename = match args.get(1).map(|s| s.as_str()) {
            Some(s) => s,
            None => $fname,
        };
        std::fs::read_to_string(filename)
            .unwrap_or_else(|_| panic!("Could not read input file {filename}"))
    }};
}
