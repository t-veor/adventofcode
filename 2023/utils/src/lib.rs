pub mod cycle_detection;
pub mod discrete_math;

#[macro_export]
macro_rules! read_input_file {
    () => {{
        let args: Vec<_> = std::env::args().collect();
        if let Some(filename) = args.get(1).map(|s| &s[..]) {
            std::borrow::Cow::from(std::fs::read_to_string(filename).unwrap())
        } else {
            std::borrow::Cow::from(include_str!("../input.txt"))
        }
    }};
}
