#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: String) -> Vec<u8> {
    input.trim().as_bytes().to_vec()
}

// Because we're skipping checkking for forbidden characters by generating only
// passwords that do not contain i, o, or l, we need to fix the initial password
// in case it has a forbidden character
fn fix_password_with_invalid_chars(password: &mut Vec<u8>) -> bool {
    for i in 0..password.len() {
        if b"iol".contains(&password[i]) {
            password[i] += 1;
            for j in i + 1..password.len() {
                password[j] = b'a';
            }
            return true;
        }
    }

    false
}

fn increment_password(password: &mut Vec<u8>) {
    for i in (0..password.len()).rev() {
        password[i] += 1;
        if b"iol".contains(&password[i]) {
            password[i] += 1;
        }
        if password[i] > b'z' {
            password[i] = b'a';
        } else {
            return;
        }
    }

    password.insert(0, b'a')
}

fn is_valid_password(password: &[u8]) -> bool {
    let mut has_straight = false;
    for i in 2..password.len() {
        if password[i - 2] + 1 == password[i - 1] && password[i - 1] + 1 == password[i] {
            has_straight = true;
            break;
        }
    }

    let mut first_double = None;
    let mut has_pairs = false;
    for i in 1..password.len() {
        if password[i - 1] == password[i] {
            match first_double {
                Some(first) if first != password[i] => {
                    has_pairs = true;
                    break;
                }
                Some(_) => (),
                None => first_double = Some(password[i]),
            }
        }
    }

    has_straight && has_pairs
}

fn star1(input: &[u8]) -> String {
    let mut password = input.to_vec();

    let has_incremented = fix_password_with_invalid_chars(&mut password);
    if has_incremented && is_valid_password(&password) {
        return String::from_utf8(password).unwrap();
    }

    loop {
        increment_password(&mut password);
        if is_valid_password(&password) {
            return String::from_utf8(password).unwrap();
        }
    }
}

fn star2(input: &[u8]) -> String {
    let mut password = star1(input).into_bytes();

    loop {
        increment_password(&mut password);
        if is_valid_password(&password) {
            return String::from_utf8(password).unwrap();
        }
    }
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
