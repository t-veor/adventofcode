use std::collections::{HashMap, VecDeque};

use utils::{discrete_math::lcm, read_input_file};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PulseType {
    Low,
    High,
}

#[derive(Debug, Clone)]
enum ModuleData {
    FlipFlop { on: bool },
    Conjunction { pulses: Vec<PulseType> },
    Broadcaster,
    Output,
}

#[derive(Debug, Clone)]
struct ModuleOutput {
    module_idx: usize,
    port_no: usize,
}

#[derive(Debug, Clone)]
struct Module {
    #[allow(unused)]
    name: String, // debugging
    data: ModuleData,
    // module_idx, port_no
    output: Vec<ModuleOutput>,
}

#[derive(Debug, Clone)]
struct Pulse {
    pulse_type: PulseType,
    #[allow(unused)]
    source: Option<usize>, // debugging
    destination: usize,
    port_no: usize,
}

#[derive(Debug, Clone)]
struct Network {
    modules: Vec<Module>,
    broadcaster_idx: usize,
}

impl Network {
    fn parse(input: &str) -> Self {
        let mut modules = Vec::new();
        let mut module_indices = HashMap::new();

        let mut broadcaster_idx = None;

        // Pass 1: collect module definitions
        for line in input.lines() {
            let (header, _) = line.split_once(" -> ").unwrap();

            let (name, module_data) = if let Some(name) = header.strip_prefix('%') {
                (name, ModuleData::FlipFlop { on: false })
            } else if let Some(name) = header.strip_prefix('&') {
                (name, ModuleData::Conjunction { pulses: Vec::new() })
            } else if header == "broadcaster" {
                broadcaster_idx = Some(modules.len());
                (header, ModuleData::Broadcaster)
            } else {
                (header, ModuleData::Output)
            };

            module_indices.insert(name.to_string(), modules.len());
            modules.push(Module {
                name: name.to_string(),
                data: module_data,
                output: Vec::new(),
            });
        }

        let mut module_input_count = HashMap::new();

        // Pass 2: wire up outputs
        for (i, line) in input.lines().enumerate() {
            let (_, outputs) = line.split_once(" -> ").unwrap();

            for output_name in outputs.split(", ") {
                let module_idx = match module_indices.get(output_name) {
                    Some(idx) => *idx,
                    None => {
                        // Need to create a new output module
                        let module_idx = modules.len();
                        module_indices.insert(output_name.to_string(), module_idx);
                        modules.push(Module {
                            name: output_name.to_string(),
                            data: ModuleData::Output,
                            output: Vec::new(),
                        });
                        module_idx
                    }
                };

                let input_count = module_input_count.entry(module_idx).or_insert(0);

                modules[i].output.push(ModuleOutput {
                    module_idx,
                    port_no: *input_count,
                });

                *input_count += 1;
            }
        }

        // Finally, ensure conjunction modules are initialised with the right no. of inputs
        for (i, module) in modules.iter_mut().enumerate() {
            match &mut module.data {
                ModuleData::Conjunction { pulses } => {
                    *pulses = vec![PulseType::Low; module_input_count.get(&i).copied().unwrap_or(0)]
                }
                _ => (),
            }
        }

        Self {
            modules,
            broadcaster_idx: broadcaster_idx.unwrap(),
        }
    }

    fn push_button(&mut self, mut watch_fn: impl FnMut(&Pulse)) -> (usize, usize) {
        let mut pulses = VecDeque::new();

        let button_pulse = Pulse {
            pulse_type: PulseType::Low,
            source: None,
            destination: self.broadcaster_idx,
            port_no: 0,
        };
        pulses.push_back(button_pulse);

        let mut low_pulses = 1;
        let mut high_pulses = 0;

        while let Some(pulse) = pulses.pop_front() {
            let pulse_delta = self.process_pulse(pulse, &mut pulses, &mut watch_fn);
            low_pulses += pulse_delta.0;
            high_pulses += pulse_delta.1;
        }

        (low_pulses, high_pulses)
    }

    #[allow(unused)]
    fn debug_pulse(&self, pulse: &Pulse) {
        let pulse_name = match pulse.pulse_type {
            PulseType::Low => "low",
            PulseType::High => "high",
        };
        let src_name = pulse
            .source
            .map(|src| self.modules[src].name.as_str())
            .unwrap_or("button");
        let dst_name = self.modules[pulse.destination].name.as_str();

        println!("{src_name} -{pulse_name}-> {dst_name}");
    }

    fn process_pulse(
        &mut self,
        pulse: Pulse,
        pulse_queue: &mut VecDeque<Pulse>,
        mut watch_fn: impl FnMut(&Pulse),
    ) -> (usize, usize) {
        let mut low_pulses = 0;
        let mut high_pulses = 0;

        let module = &mut self.modules[pulse.destination];
        let mut send_pulse = |pulse_type: PulseType| {
            for output in module.output.iter() {
                pulse_queue.push_back(Pulse {
                    pulse_type,
                    source: Some(pulse.destination),
                    destination: output.module_idx,
                    port_no: output.port_no,
                });
            }

            match pulse_type {
                PulseType::Low => low_pulses += module.output.len(),
                PulseType::High => high_pulses += module.output.len(),
            }
        };

        match &mut module.data {
            ModuleData::FlipFlop { on } => match pulse.pulse_type {
                PulseType::High => (),
                PulseType::Low => {
                    *on = !*on;
                    send_pulse(match *on {
                        false => PulseType::Low,
                        true => PulseType::High,
                    })
                }
            },
            ModuleData::Conjunction { pulses } => {
                pulses[pulse.port_no] = pulse.pulse_type;
                if pulses.iter().all(|&p| p == PulseType::High) {
                    send_pulse(PulseType::Low);
                } else {
                    send_pulse(PulseType::High);
                }
            }
            ModuleData::Broadcaster => send_pulse(pulse.pulse_type),
            ModuleData::Output => (),
        }

        watch_fn(&pulse);

        (low_pulses, high_pulses)
    }
}

fn star1(mut network: Network) -> u64 {
    let mut low_pulses = 0;
    let mut high_pulses = 0;

    for _ in 0..1000 {
        let pulse_delta = network.push_button(|_| ());
        low_pulses += pulse_delta.0;
        high_pulses += pulse_delta.1;
    }

    low_pulses as u64 * high_pulses as u64
}

fn star2(mut network: Network) -> i64 {
    // I'm pretty sure it's not possible to write an efficient solution which
    // works for all possible networks. The way the conjuction nodes implement
    // NAND and flip-flops implement storage makes the system look pretty
    // Turing-complete...

    // Instead, we have to make some assumptions. By looking at my input it
    // looks like the modules are configured in this arrangement:
    //
    // broadcaster
    //  |
    //  +--> (chain of flip-flops and a conjunction node, which
    //  |     sends a low pulse every n0 ticks) --> &inv0 -------+
    //  |                                                        |
    //  +--> (chain of flip-flops and a conjunction node, which  |
    //  |     sends a low pulse every n1 ticks) --> &inv1 -------+
    //  |                                                        |
    //  +--> (chain of flip-flops and a conjunction node, which  |
    //  |     sends a low pulse every n2 ticks) --> &inv2 -------+
    //  |                                                        |
    //  +--> (chain of flip-flops and a conjunction node, which  |
    //        sends a low pulse every n3 ticks) --> &inv3 -------+
    //                                                           |
    //                                                           v
    //                                                          final_nand
    //                                                           |
    //                                                           v
    //                                                          rx

    // Therefore, rx will first receive a low pulse when all the &invx modules
    // pulse high on the same tick, which is lcm(n0, n1, ...) ticks. I had
    // manually determined the cycle lengths by looking at the node
    // configurations, but if we assume all inputs follow the same structure,
    // it's possible to automatically determine the cycle lengths simply by
    // watching when the &invx modules pulse high.

    // Find the idx of the rx module
    let rx_idx = network
        .modules
        .iter()
        .position(|module| module.name == "rx")
        .expect("Couldn't find rx module");

    // Find the idx of the final_nand module
    let final_nand_idx = network
        .modules
        .iter()
        .position(|module| {
            matches!(module.data, ModuleData::Conjunction { .. })
                && module.output.len() == 1
                && module.output[0].module_idx == rx_idx
        })
        .expect("Couldn't find module representing the final NAND before rx");

    // Find the indices of the inverter modules
    let mut inverters: Vec<_> = network
        .modules
        .iter()
        .enumerate()
        .filter_map(|(idx, module)| {
            (matches!(module.data, ModuleData::Conjunction { .. })
                && module.output.len() == 1
                && module.output[0].module_idx == final_nand_idx)
                .then_some(idx)
        })
        .collect();

    let mut cycle_lengths = Vec::new();

    // Simulate the network, watching for when the inverters first pulse high
    for pushes in 1.. {
        network.push_button(|pulse| match pulse {
            Pulse {
                pulse_type: PulseType::High,
                source: Some(src),
                ..
            } if inverters.contains(src) => {
                cycle_lengths.push(pushes);
                inverters.retain(|i| i != src);
            }
            _ => (),
        });

        if inverters.is_empty() {
            break;
        }
    }

    cycle_lengths.into_iter().fold(1, lcm)
}

fn main() {
    let input = read_input_file!();
    let network = Network::parse(&input);

    println!("{}", star1(network.clone()));
    println!("{}", star2(network));
}
