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
shebang and comment for this to work. Since some of the Haskell solutions are
quite slow when run in interpreted mode, you can run
`stack --resolver lts-14.16 script filename.hs --optimize` to produce a much
faster executable.

### Rust

I use [`rust-script`](https://github.com/fornwall/rust-script) to run Rust files
as scripts. After installing by running `cargo install rust-script`, `cd` to the
correct directory and run `cargo script filename.rs`. There are some helper
scripts in the `scripts/` directory to help set up rust-analyzer for these
script files.

### C

There's a bit of hackery to get the C files to also execute as a bash script
that compiles and executes itself if you run `./filename.c`, but otherwise they
are self-contained and you should be able to just run
`gcc filename.c && ./a.out` in the right directory (other compilers are
available).
