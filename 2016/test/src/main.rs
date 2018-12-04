extern crate regex;
extern crate md5;

use std::io::prelude::*;
use std::fs::File;
use regex::Regex;
use std::str::FromStr;
use std::collections::HashMap;
use std::env;
use std::char;
use md5::*;

fn main() {
    /*
    let name = env::args().nth(1).unwrap();
    let mut f = File::open(name).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    solve1(&s.as_str());
    solve2(&s.as_str());
    */
    let mut n = 3231929;
    let mut found = 0;
    while found < 8 {
        let key = format!("abc{}", n);
        let d = md5::compute(key.as_bytes());
        print!("hashed {} to {:?}\n", key, d);
        if(d[0] == 0 && d[1] == 0 && d[2] <= 0xF) {
            print!("{:x}\n", d[2]);
            found += 1;
        }
        n += 1;
    }
}
