extern crate regex;

use std::io::prelude::*;
use std::fs::File;
use regex::Regex;
use std::str::FromStr;
use std::collections::HashSet;

fn solve1(s: &str) -> () {
    let mut n = 0;
    let mut total = 0;
    let re = Regex::new(r"(\d+) +(\d+) + (\d+)\n").unwrap();
    for m in re.captures_iter(s) {
        total += 1;
        let (x, y, z) = (i64::from_str(&m[1]).unwrap(), i64::from_str(&m[2]).unwrap(), i64::from_str(&m[3]).unwrap()); 
        if (x + y <= z) || (x + z <= y) || (y + z <= x) {
            print!("{} {} {} doesn't work\n", x, y, z);
        }
        else {
            print!("{} {} {} works\n", x, y, z);
            n += 1;
        }
    }
    print!("{} out of {}\n", n, total);
}

fn solve2(s: &str) -> () {
    let mut n = 0;
    let mut total = 0;
    let re = Regex::new(r"(\d+) +(\d+) + (\d+)\n").unwrap();
    let mut i = re.captures_iter(s);
    loop {
        if let Some(fm) = i.next() {
            let sm = i.next().unwrap();
            let tm = i.next().unwrap();
            let (x1, y1, z1) = (i64::from_str(&fm[1]).unwrap(), i64::from_str(&sm[1]).unwrap(), i64::from_str(&tm[1]).unwrap()); 
            let (x2, y2, z2) = (i64::from_str(&fm[2]).unwrap(), i64::from_str(&sm[2]).unwrap(), i64::from_str(&tm[2]).unwrap()); 
            let (x3, y3, z3) = (i64::from_str(&fm[3]).unwrap(), i64::from_str(&sm[3]).unwrap(), i64::from_str(&tm[3]).unwrap()); 
            if (x1 + y1 <= z1) || (x1 + z1 <= y1) || (y1 + z1 <= x1) {
                print!("{} {} {} doesn't work\n", x1, y1, z1);
            }
            else {
                print!("{} {} {} works\n", x1, y1, z1);
                n += 1;
            }
            if (x3 + y3 <= z3) || (x3 + z3 <= y3) || (y3 + z3 <= x3) {
                print!("{} {} {} doesn't work\n", x3, y3, z3);
            }
            else {
                print!("{} {} {} works\n", x3, y3, z3);
                n += 1;
            }
            if (x2 + y2 <= z2) || (x2 + z2 <= y2) || (y2 + z2 <= x2) {
                print!("{} {} {} doesn't work\n", x2, y2, z2);
            }
            else {
                print!("{} {} {} works\n", x2, y2, z2);
                n += 1;
            }
            total += 3;
        }
        else {
            break;
        }
    }
    print!("{} out of {}\n", n, total);
}

fn main() {
    let mut f = File::open("input.txt").unwrap();
    let mut s = String::new();
    let mut n = f.read_to_string(&mut s).unwrap();
    solve1(&s.as_str());
    solve2(&s.as_str());
}
