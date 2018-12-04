
use std::io::prelude::*;
use std::fs::File;

fn clamp(n: i64, (l, u): (i64, i64)) -> i64 {
    if n < l {
        return l;
    }
    if n > u {
        return u;
    }
    n
}
fn bounds(n: i64) -> (i64, i64) {
    let w = (2 - n).abs();
    (w, 4 - w)
}

fn main() {
    let vals = [['X', 'X', '1', 'X', 'X'],
                ['X', '2', '3', '4', 'X'],
                ['5', '6', '7', '8', '9'],
                ['X', 'A', 'B', 'C', 'X'],
                ['X', 'X', 'D', 'X', 'X']];
    let mut f = File::open("input.txt").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    let mut x = 0i64;
    let mut y = 2i64;
    for c in s.chars() {
        match c {
            'L' => x = clamp(x - 1, bounds(y)),
            'R' => x = clamp(x + 1, bounds(y)),
            'U' => y = clamp(y - 1, bounds(x)),
            'D' => y = clamp(y + 1, bounds(x)),
            '\n' => print!("{}", vals[y as usize][x as usize]),
            _ => panic!("wut"),
        }
    }
    print!("\n");
}
