import json
import glob
import os
import subprocess

WORKSPACE_DIR = os.path.join(os.path.dirname(__file__), "..")
os.chdir(WORKSPACE_DIR)

rust_project_json = {}
try:
    with open("rust-project.json") as f:
        rust_project_json = json.load(f)
except OSError:
    pass

if "sysroot_src" not in rust_project_json:
    rustc_process = subprocess.run(["rustc", "--print", "sysroot"], capture_output=True, text=True)
    rust_project_json["sysroot_src"] = os.path.join(rustc_process.stdout.rstrip(), "/lib/rustlib/src/rust/library")

if "crates" not in rust_project_json:
    rust_project_json["crates"] = []

crates = rust_project_json["crates"]
existing_crate_roots = set(crate["root_module"] for crate in crates)

rust_files = glob.glob("**/day*.rs", recursive=True)
files_to_add = set(rust_files) - existing_crate_roots
for file in sorted(files_to_add):
    crates.append({
        "root_module": file,
        "edition": "2021",
        "deps": []
    })

with open("rust-project.json", "w+") as f:
    json.dump(rust_project_json, f, indent=4)
