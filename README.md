# Advent of Code Solutions

Just a place where I can dump my Advent of Code solutions.

## Running solutions

On systems that support the shebang, you should be able to just do
`./filename.ext` to execute solution files as a script.

### Python
I run my Python files using `python` in the terminal. Simply `cd` to the correct
directory and run the script as `python filename.py`.

### Haskell
I run my Haskell files as scripts with
[`stack`](https://www.haskellstack.org/), by `cd`-ing to the right directory
and running `stack filename.hs`. The Haskell files are set up with the right
shebang and comment for this to work.

### Rust
I use [`cargo-script`](https://github.com/DanielKeep/cargo-script) to run Rust
files as scripts. After installing `cargo-script`, `cd` to the correct directory
and run `cargo script filename.rs`.

### C
There's a bit of hackery to get the C files to also execute as a bash script
that compiles itself and executes it if you run `./filename.c`, but otherwise
they are self-contained and you should be able to just run
`gcc filename.c && ./a.out` in the right directory (other compilers are
available).
