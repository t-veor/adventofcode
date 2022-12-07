#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Handle(usize);

#[derive(Debug)]
enum DirEntry {
    File {
        name: String,
        size: usize,
    },
    Directory {
        name: String,
        parent: Option<Handle>,
        children: Vec<Handle>,
    },
}

#[derive(Debug)]
struct FileSystem {
    entries: Vec<DirEntry>,
}

impl FileSystem {
    fn new() -> Self {
        Self {
            entries: vec![DirEntry::Directory {
                name: "/".to_string(),
                parent: None,
                children: Vec::new(),
            }],
        }
    }

    const fn root_handle() -> Handle {
        Handle(0)
    }

    fn entry(&self, handle: Handle) -> Option<&DirEntry> {
        self.entries.get(handle.0)
    }

    fn cd_relative(&self, cwd: Handle, dir_name: &str) -> Option<Handle> {
        let (parent, children) = match self.entries.get(cwd.0) {
            Some(DirEntry::Directory {
                parent, children, ..
            }) => (parent, children),
            _ => return None,
        };

        if dir_name == ".." {
            *parent
        } else {
            for (handle, child) in children
                .iter()
                .filter_map(|i| Some((*i, self.entries.get(i.0)?)))
            {
                match child {
                    DirEntry::Directory { name, .. } if name == dir_name => return Some(handle),
                    _ => (),
                }
            }

            None
        }
    }

    fn add_file(&mut self, cwd: Handle, name: &str, size: usize) -> Option<Handle> {
        let handle = Handle(self.entries.len());
        if let Some(DirEntry::Directory { children, .. }) = self.entries.get_mut(cwd.0) {
            children.push(handle);
            self.entries.push(DirEntry::File {
                name: name.to_string(),
                size,
            });
            Some(handle)
        } else {
            None
        }
    }

    fn add_directory(&mut self, cwd: Handle, name: &str) -> Option<Handle> {
        let handle = Handle(self.entries.len());
        if let Some(DirEntry::Directory { children, .. }) = self.entries.get_mut(cwd.0) {
            children.push(handle);
            self.entries.push(DirEntry::Directory {
                name: name.to_string(),
                parent: Some(cwd),
                children: Vec::new(),
            });
            Some(handle)
        } else {
            None
        }
    }
}

fn parse_input(input: String) -> FileSystem {
    let mut fs = FileSystem::new();
    let mut cwd = FileSystem::root_handle();

    let mut lines = input.lines().peekable();
    while let Some(line) = lines.next() {
        if line.starts_with("$ cd ") {
            let dir_name = &line["$ cd ".len()..];
            if dir_name == "/" {
                cwd = FileSystem::root_handle();
            } else {
                cwd = fs.cd_relative(cwd, dir_name).unwrap();
            }
        } else if line.starts_with("$ ls") {
            loop {
                match lines.peek() {
                    Some(line) if !line.starts_with("$") => {
                        let (dat, name) = line.split_once(' ').unwrap();
                        if dat == "dir" {
                            fs.add_directory(cwd, name).unwrap();
                        } else if let Ok(size) = dat.parse::<usize>() {
                            fs.add_file(cwd, name, size).unwrap();
                        } else {
                            unreachable!()
                        }
                        lines.next();
                    }
                    _ => break,
                }
            }
        } else {
            break;
        }
    }

    fs
}

fn get_size_recursive<F>(fs: &FileSystem, handle: Handle, dir_hook: &mut F) -> usize
where
    F: FnMut(usize),
{
    let size = match fs.entry(handle) {
        Some(DirEntry::Directory { children, .. }) => {
            let mut sum = 0;
            for child in children {
                sum += get_size_recursive(fs, *child, dir_hook);
            }

            dir_hook(sum);

            sum
        }
        Some(DirEntry::File { size, .. }) => *size,
        None => 0,
    };

    size
}

fn star1(input: &FileSystem) -> usize {
    let mut total = 0;
    get_size_recursive(input, FileSystem::root_handle(), &mut |dir_size| {
        if dir_size <= 100000 {
            total += dir_size;
        }
    });

    total
}

fn star2(input: &FileSystem) -> usize {
    let total_size = get_size_recursive(input, FileSystem::root_handle(), &mut |_| ());
    let max_allowed_size = 70000000 - 30000000;
    assert!(total_size > max_allowed_size);
    let min_candidate_size = total_size - max_allowed_size;

    let mut candidate = usize::MAX;
    get_size_recursive(input, FileSystem::root_handle(), &mut |dir_size| {
        if dir_size >= min_candidate_size && dir_size < candidate {
            candidate = dir_size;
        }
    });

    candidate
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
