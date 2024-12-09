use std::ops::Range;

use utils::read_input_file;

#[derive(Debug, Clone)]
struct FileSystem {
    total_size: usize,
    files: Vec<(u32, Range<usize>)>,
    free_space: Vec<Range<usize>>,
}

impl FileSystem {
    fn parse(input: &str) -> Self {
        let mut files = Vec::new();
        let mut free_space: Vec<Range<usize>> = Vec::new();
        let mut total_size = 0;

        let mut is_file = true;
        let mut file_id = 0;

        for char in input.trim().chars() {
            let length = (char as u32 - '0' as u32) as usize;
            if is_file {
                files.push((file_id, total_size..total_size + length));
                file_id += 1;
            } else {
                // Make sure to merge contiguous free space
                match free_space.last_mut() {
                    Some(last) if last.end == total_size => last.end = total_size + length,
                    _ => free_space.push(total_size..total_size + length),
                }
            }
            total_size += length;
            is_file = !is_file;
        }

        Self {
            total_size,
            files,
            free_space,
        }
    }

    // Part 2 compaction
    fn compact(&mut self) {
        // Iterate backwards through th files...
        for (_file_id, file) in self.files.iter_mut().rev() {
            // Scan through free space for enough continguous space
            for free_space in self.free_space.iter_mut() {
                // Don't move a file after itself
                if free_space.start > file.start {
                    break;
                }

                if free_space.len() >= file.len() {
                    // enough space, move into the beginning of this range
                    // Adjust the range on this file
                    *file = free_space.start..free_space.start + file.len();
                    // Adjust the remaining free space left
                    *free_space = file.end..free_space.end;
                    // (We don't care about if this makes free_space len 0)
                    break;
                }
            }
        }
    }

    fn checksum(&self) -> u64 {
        self.files
            .iter()
            .flat_map(|(file_id, file)| file.clone().map(|i| i as u64 * *file_id as u64))
            .sum()
    }
}

#[derive(Debug, Clone)]
struct FileSystemSimple {
    blocks: Vec<Option<u32>>,
    free_list: Vec<usize>,
    last_file_idx: usize,
}

impl FileSystemSimple {
    fn from_filesystem(file_system: &FileSystem) -> Self {
        let mut free_list = Vec::new();
        let mut blocks = vec![None; file_system.total_size];
        let mut last_file_idx = 0;

        for (file_id, file_range) in file_system.files.iter() {
            for i in file_range.clone() {
                blocks[i] = Some(*file_id);
                last_file_idx = i;
            }
        }

        for free_range in file_system.free_space.iter() {
            for i in free_range.clone() {
                free_list.push(i);
            }
        }

        free_list.reverse();

        Self {
            free_list,
            blocks,
            last_file_idx,
        }
    }

    // Part 1 compaction
    fn compact_end(&mut self) -> bool {
        if let Some(idx) = self.free_list.pop() {
            debug_assert!(self.blocks[idx].is_none());

            let file_id = loop {
                let last = *self.blocks.get(self.last_file_idx).expect("Disk is empty");
                match last {
                    Some(file_id) => break file_id,
                    None => self.last_file_idx -= 1,
                }
            };

            if idx >= self.last_file_idx {
                return false;
            }

            self.blocks[idx] = Some(file_id);
            self.blocks[self.last_file_idx] = None;

            true
        } else {
            false
        }
    }

    fn checksum(&self) -> u64 {
        self.blocks
            .iter()
            .enumerate()
            .map(|(i, block)| match block {
                Some(x) => i as u64 * *x as u64,
                None => 0,
            })
            .sum()
    }

    #[allow(unused)]
    fn debug_print(&self) {
        for i in self.blocks.iter().copied() {
            match i {
                Some(x) if x >= 10 => print!("[{x}]"),
                Some(x) => print!("{x}"),
                None => print!("."),
            }
        }
        println!()
    }
}

fn parse_input(input: &str) -> FileSystem {
    FileSystem::parse(input)
}

fn star1(file_system: &FileSystem) -> u64 {
    let mut file_system = FileSystemSimple::from_filesystem(file_system);

    loop {
        if !file_system.compact_end() {
            break;
        }
    }

    file_system.checksum()
}

fn star2(file_system: &FileSystem) -> u64 {
    let mut file_system = file_system.clone();
    file_system.compact();
    file_system.checksum()
}

fn main() {
    let input = read_input_file!();
    let file_system = parse_input(&input);

    println!("{}", star1(&file_system));
    println!("{}", star2(&file_system));
}
