use utils::{pathfinding::dijkstra, read_input_file};

#[derive(Debug, Clone)]
struct RegFile {
    a: i64,
    b: i64,
    c: i64,
}

#[derive(Debug, Clone, Copy)]
enum Instr {
    Adv,
    Bxl,
    Bst,
    Jnz,
    Bxc,
    Out,
    Bdv,
    Cdv,
}

fn fetch_decode(regfile: &RegFile, instrs: &[i64], pc: i64) -> Option<(Instr, i64)> {
    let pc: usize = pc.try_into().ok()?;
    let instr = instrs.get(pc)?;
    let operand = *instrs.get(pc + 1)?;

    let (instr, is_combo) = match instr {
        0 => (Instr::Adv, true),
        1 => (Instr::Bxl, false),
        2 => (Instr::Bst, true),
        3 => (Instr::Jnz, false),
        4 => (Instr::Bxc, false),
        5 => (Instr::Out, true),
        6 => (Instr::Bdv, true),
        7 => (Instr::Cdv, true),
        _ => panic!("Unknown opcode {instr} encountered"),
    };

    let operand = match operand {
        _ if !is_combo => operand,
        0..=3 => operand,
        4 => regfile.a,
        5 => regfile.b,
        6 => regfile.c,
        7 => panic!("Reserved combo operand encountered"),
        _ => panic!("Unknown combo operand {operand} encountered"),
    };

    Some((instr, operand))
}

fn run(mut regfile: RegFile, instrs: &[i64]) -> Vec<i64> {
    let mut pc = 0;
    let mut output = Vec::new();

    while let Some((instr, operand)) = fetch_decode(&regfile, instrs, pc) {
        match instr {
            Instr::Adv => regfile.a /= 1 << operand,
            Instr::Bxl => regfile.b ^= operand,
            Instr::Bst => regfile.b = operand % 8,
            Instr::Jnz => {
                if regfile.a != 0 {
                    pc = operand - 2;
                }
            }
            Instr::Bxc => regfile.b ^= regfile.c,
            Instr::Out => output.push(operand % 8),
            Instr::Bdv => regfile.b = regfile.a / (1 << operand),
            Instr::Cdv => regfile.c = regfile.a / (1 << operand),
        }

        pc += 2;
    }

    output
}

fn parse_input(input: &str) -> (RegFile, Vec<i64>) {
    let (registers, instrs) = input.trim().split_once("\n\n").unwrap();

    let registers = {
        let [a, b, c] = registers
            .lines()
            .map(|line| line.split_once(": ").unwrap().1.parse().unwrap())
            .collect::<Vec<_>>()[..]
        else {
            panic!()
        };
        RegFile { a, b, c }
    };

    let instrs = instrs
        .split_once(": ")
        .unwrap()
        .1
        .split(',')
        .map(|i| i.parse().unwrap())
        .collect();

    (registers, instrs)
}

fn star1(regfile: &RegFile, instrs: &[i64]) -> String {
    run(regfile.clone(), instrs)
        .into_iter()
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(",")
}

fn star2(instrs: &[i64]) -> i64 {
    // From reverse engineering my input, it appears to be doing this:
    // 2 4; bst a
    // 1 1; bxl 1
    // 7 5; cdv b
    // 1 4; bxl 4
    // 0 3; adv 8
    // 4 5; bxc
    // 5 5; out b
    // 3 0; jnz 0
    // Which is the equivalent of this:
    // while a != 0 {
    //     let b1 = a % 8 ^ 1;
    //     let c = a / (1 << b1);
    //     let b2 = b1 ^ c ^ 4;
    //     output(b2 % 8);
    //     a /= 8;
    // }
    // So it looks likes like it's processing a 3 bits of A at a time, with each
    // iteration possibly depending on last 11 bits of A.
    // Thus, we can do a search on the value of A, 3 bits at a time, each time
    // looking to see if we can extend the length of the correct prefix of the
    // output until it fully matches the list of instructions. This is hopefully
    // general enough to capture all the possible inputs for this program.

    // My first approach was to just try to fix the value of a 3 bits at a time,
    // where I search 2^11 possible prefixes concat'd with any bits I've fixed
    // already, and then concat the prefix that works onto the current fixed
    // value. This produced a value that works, but it was too high so was
    // clearly not the minimum value that works. This must have happened because
    // earlier I made a greedy assumption to fix some bits when another option
    // would also work, but I didn't search down that path.

    // So then I converted it into a graph-search problem, where each node
    // represents a fixed part of A that could generate part of the input, and
    // the children are possible prefixes that could extend the correct part of
    // the input. Then the search just needs to find a path to any node where
    // the fixed part generates the entirety of the input, minimising the node
    // value.

    // Dijkstra is basically only used as a priority queue to explore nodes with
    // a lower fixed A first. But hey, I've got this utility library and it
    // works, so...
    dijkstra(
        (0, 0),
        |&(a, _)| run(RegFile { a, b: 0, c: 0 }, instrs) == instrs,
        |&(fixed_bits, fixed_segments)| {
            let fixed_bits_count = fixed_segments * 3;
            let mut candidates = [false; 8];

            for new_bits in 0..(1 << 11) {
                if candidates[new_bits as usize % 8] {
                    continue;
                }

                let a = fixed_bits | (new_bits << fixed_bits_count);
                let output = run(RegFile { a, b: 0, c: 0 }, instrs);
                if output.len() >= fixed_segments
                    && output[..fixed_segments] == instrs[..fixed_segments]
                {
                    candidates[new_bits as usize % 8] = true;
                }
            }

            candidates
                .into_iter()
                .enumerate()
                .filter_map(|(i, is_candidate)| {
                    is_candidate.then(|| {
                        let new_val = fixed_bits | ((i as i64) << fixed_bits_count);
                        (new_val - fixed_bits, (new_val, fixed_segments + 1))
                    })
                })
                .collect()
        },
    )
    .unwrap()
    .0
}

fn main() {
    let input = read_input_file!();
    let (regfile, instrs) = parse_input(&input);

    println!("{}", star1(&regfile, &instrs));
    println!("{}", star2(&instrs));
}
