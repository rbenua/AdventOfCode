extern crate regex;

use std::io::prelude::*;
use std::fs::File;
use regex::Regex;
use std::str::FromStr;
use std::collections::HashSet;

fn main() {
    let mut f = File::open("input.txt").unwrap();
    let mut s = String::new();
    let mut n = f.read_to_string(&mut s).unwrap();
    print!("{} {}", n, s);
    let re = Regex::new(r"([LR])(\d+)").unwrap();
    let mut facing = 0u32;
    let mut x = 0i64;
    let mut y = 0i64;
    let mut set = HashSet::new();
    set.insert((0, 0));

    for cap in re.captures_iter(s.as_str()) {
        let len = i64::from_str(&cap[2]).unwrap();

        print!("{}: start is {}, {}, facing {}", &cap[0], x, y, facing);
        facing = match (&cap[1]) {
            "L" => (facing - 1) % 4,
            "R" => (facing + 1) % 4,
            _ => panic!("lol"),
        };
        for _ in 0..len {
            match facing {
                0 => y += 1,
                1 => x += 1,
                2 => y -= 1,
                3 => x -= 1,
                _ => panic!("wut, facing = {}", facing),
            }
            if !set.insert((x, y)) {
                print!("{}, {}, {}\n", x, y, x.abs() + y.abs());
                return;
            }
        }

        print!(", new facing is {}, len {}, end position {}, {}\n", facing, len, x, y);
    }
}
