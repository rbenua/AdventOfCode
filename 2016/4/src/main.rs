extern crate regex;

use std::io::prelude::*;
use std::fs::File;
use regex::Regex;
use std::str::FromStr;
use std::collections::HashMap;
use std::env;
use std::char;

fn rotate(c: char, r: u32) -> char {
    if c == '-' {
        ' '
    }
    else {
        let a = 'a' as u32;
        let i = ((c as u32) - a + r) % 26 + a;
        char::from_u32(i).unwrap()
    }
}

fn solve1(s: &str) -> () {
    let mut n = 0;
    let re = Regex::new(r"([a-z-]+)(\d+)\[(\w+)\]\n").unwrap();
    for m in re.captures_iter(s) {
        let mut map = HashMap::new();
        for c in m[1].chars() {
            let e = map.entry(c).or_insert(0);
            *e += 1;
        }
        map.remove(&'-');
        let mut pairs = map.drain().collect::<Vec<_>>();
        pairs.sort_by(|&(k1, v1), &(k2, v2)| {
            if v1.eq(&v2) {
                return k1.cmp(&k2);
            }
            v2.cmp(&v1)
        });
        let s: String = pairs.into_iter().map(|(k, _)| k).take(5).collect();
        if s.as_str() == &m[3] {
            let id = u32::from_str(&m[2]).unwrap();
            //print!("{} matches, adding {}\n", &m[0], id);
            n += id;
            let rotated: String = m[1].chars().map(|c| rotate(c, id)).collect();
            print!("{} {}\n", rotated, id);
        }
        else {
            //print!("{} ({} {}) doesn't match {}, expected checksum {}\n", &m[0], &m[1], &m[2], &m[3], s);
        }
    }
    print!("Part 1: Total is {}\n", n);
}

fn main() {
    let name = env::args().nth(1).unwrap();
    let mut f = File::open(name).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    solve1(&s.as_str());
}
