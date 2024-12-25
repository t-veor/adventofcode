use utils::read_input_file;

fn parse_input(input: &str) -> (Vec<[u32; 5]>, Vec<[u32; 5]>) {
    enum SchematicType {
        Lock,
        Key,
    }

    fn parse_schematic(schematic: &str) -> (SchematicType, [u32; 5]) {
        let lines: Vec<_> = schematic.lines().collect();
        let mut counts = [0; 5];

        for line in lines[1..lines.len() - 1].iter() {
            for (i, c) in line.chars().enumerate() {
                if c == '#' {
                    counts[i] += 1;
                }
            }
        }

        let schematic_type = if lines[0].contains('#') {
            SchematicType::Lock
        } else {
            SchematicType::Key
        };

        (schematic_type, counts)
    }

    let mut locks = Vec::new();
    let mut keys = Vec::new();
    for schematic in input.split("\n\n") {
        let (schematic_type, counts) = parse_schematic(schematic);
        match schematic_type {
            SchematicType::Lock => locks.push(counts),
            SchematicType::Key => keys.push(counts),
        }
    }

    (locks, keys)
}

fn star1(locks: &[[u32; 5]], keys: &[[u32; 5]]) -> u32 {
    let mut total = 0;

    for lock in locks {
        for key in keys {
            if lock.iter().zip(key).map(|(a, b)| a + b).all(|x| x <= 5) {
                total += 1;
            }
        }
    }

    total
}

fn main() {
    let input = read_input_file!();
    let (locks, keys) = parse_input(&input);

    println!("{}", star1(&locks, &keys));
}
