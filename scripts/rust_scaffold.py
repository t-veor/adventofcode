import json
import sys
import os
from pathlib import Path

TEMPLATE = """#!/usr/bin/env rust-script
fn parse_input(input: String) -> () {
}

fn star1(input: &()) -> () {
    todo!()
}

fn star2(input: &()) -> () {
    todo!()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
"""

WORKSPACE_DIR = os.path.join(os.path.dirname(__file__), "..")
os.chdir(WORKSPACE_DIR)

try:
    year = int(sys.argv[1])
    day = int(sys.argv[2])
except:
    print("USAGE:", sys.argv[0], "[YEAR]", "[DAY]")
    sys.exit(1)

try:
    with open("rust-project.json") as f:
        rust_project_json = json.load(f)
        crates = rust_project_json["crates"]
        assert type(crates) is list
except:
    print("Invalid rust-project.json, try running init_rust_analyzer.py first")
    sys.exit(1)

day_name = f"day{day:02}"
day_dir = Path(str(year), day_name)
rust_file = Path(day_dir, f"{day_name}.rs")
input_file = Path(day_dir, f"input.txt")

day_dir.mkdir(parents=True, exist_ok=True)

with open(rust_file, "w+") as f:
    f.write(TEMPLATE)
input_file.touch()

root_module = str(rust_file)
if not any(crate["root_module"] == root_module for crate in crates):
    crates.append({
        "root_module": root_module,
        "edition": "2021",
        "deps": []
    })
    with open("rust-project.json", "w") as f:
        json.dump(rust_project_json, f, indent=4)
