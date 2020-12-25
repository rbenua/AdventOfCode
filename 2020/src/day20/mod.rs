use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Copy, Clone, Debug)]
struct Tile {
    id: u64,
    data: [[bool; 10]; 10],
}

pub struct Day20{
    tiles: HashMap<u64, Tile>,
    matches: HashMap<u64, Vec<u64>>,
    corners: Vec<u64>,
}

fn to_u64(bs: &[bool; 10]) -> u64 {
    bs.iter().enumerate().map(|(i, v)| if *v { 1 << i } else { 0 }).sum()
}
fn to_u64_rev(bs: &[bool; 10]) -> u64 {
    bs.iter().rev().enumerate().map(|(i, v)| if *v { 1 << i } else { 0 }).sum()
}

fn reverse<T>(input: &[T; 10]) -> [T; 10] where T: Copy {
    let indices = [9,8,7,6,5,4,3,2,1,0]; // there ought to be a cleaner way to do this...
    indices.map(|x|input[x])
}

fn rotate<T>(input: &[[T; 10]; 10]) -> [[T; 10]; 10] where T: Copy {
    let mut output: [[T; 10]; 10] = [[input[0][0]; 10]; 10];
    for i in 0..10 {
        for j in 0..10 {
            output[9-j][i] = input[i][j];
        }
    }
    output
}

fn rotate_vec<T>(input: &Vec<Vec<T>>) -> Vec<Vec<T>> where T: Copy {
    (0..input[0].len()).map(|i|{
        input.iter().map(|r|r[input[0].len() - i - 1]).collect()
    }).collect()
}

fn top<T>(input: &[[T; 10]; 10]) -> [T; 10] where T: Copy {
    input[0]
}
fn bottom<T>(input: &[[T; 10]; 10]) -> [T; 10] where T: Copy {
    input[9]
}
fn left<T>(input: &[[T; 10]; 10]) -> [T; 10] where T: Copy {
    input.map(|r|r[0])
}
fn right<T>(input: &[[T; 10]; 10]) -> [T; 10] where T: Copy {
    input.map(|r|r[9])
}

fn flip_to_match<T>(data: &[[T; 10]; 10], select: &Fn(&[[T; 10]; 10]) -> [T; 10], pattern: &[T; 10]) -> [[T; 10]; 10]where T: Copy + Eq {
    let mut d = *data;
    let mut i = 0;
    while select(&d) != *pattern {
        d = rotate(&d);
        i += 1;
        if i == 4 {
            d = reverse(&d);
        }
        if i >= 8 {
            panic!("Didn't find a working rotation");
        }
    }
    d
}

impl Day20 { 
    fn stitch_tiles(&self) -> Vec<Vec<bool>> {
        let tile_dim = (self.tiles.len() as f64).sqrt() as usize;
        let mut map_tiles: Vec<Vec<Tile>> = Vec::with_capacity(tile_dim);
        
        let mut starting_tile = self.tiles[&self.corners[0]];
        while self.matches[&to_u64(&top(&starting_tile.data))].len() > 1 ||
              self.matches[&to_u64(&left(&starting_tile.data))].len() > 1 {
            starting_tile.data = rotate(&starting_tile.data);
        }
        let mut pattern = bottom(&starting_tile.data);
        let mut curr_id = starting_tile.id;
        map_tiles.push(vec![starting_tile]);
        
        for _ in 0..(tile_dim - 1) {
            let p = to_u64(&pattern);
            let mut t = self.tiles[&self.matches[&p][0]];
            if t.id == curr_id {
                t = self.tiles[&self.matches[&p][1]];
            }
            t.data = flip_to_match(&t.data, &top, &pattern);
            pattern = bottom(&t.data);
            curr_id = t.id;
            map_tiles.push(vec![t]);
        }
        
        for row in 0..tile_dim {
            let start = map_tiles[row][0];
            let mut vpat = right(&start.data);
            let mut row_id = start.id;

            for _ in 0..(tile_dim - 1) {
                let v = to_u64(&vpat);
                let mut t = self.tiles[&self.matches[&v][0]];
                if t.id == row_id {
                    t = self.tiles[&self.matches[&v][1]];
                }
                t.data = flip_to_match(&t.data, &left, &vpat);
                vpat = right(&t.data);
                row_id = t.id;
                map_tiles[row].push(t);
            }
        }

        let mut res = Vec::with_capacity(tile_dim * 8);
        for tile_row in &map_tiles {
            for r in 1..9 {
                let mut row = Vec::with_capacity(tile_dim * 8);
                for tile in tile_row {
                    row.extend_from_slice(&tile.data[r][1..9]);
                }
                res.push(row);
            }
        }
        res
    }
}
const monster_pat: [(usize, usize); 15] = [(0,1), (1,0), (4,0), (5,1), (6,1), (7,0), (10,0), (11,1), 
                                           (12,1), (13,0), (16,0), (17,1), (18,1), (19,1), (18,2)];

fn check_loc(map: &Vec<Vec<bool>>, x: usize, y: usize) -> bool {
    monster_pat[..].iter().all(|&(dx, dy)|map[y+dy][x+dx])
}

fn search(map: &Vec<Vec<bool>>) -> Option<usize> {
    let mut monsters = 0;
    for y in 0..(map.len() - 2) {
        for x in 0..(map[0].len() - 19) {
            if check_loc(map, x, y) {
                monsters += monster_pat.len();
            }
        }
    }
    if monsters == 0 {
        None
    }
    else {
        Some(map.iter().flatten().filter(|b|**b).count() - monsters)
    }
}

pub fn setup(_input:&str) -> Result<Day20, Box<dyn Error>>{
    let mut res = Day20{
        tiles: HashMap::new(),
        matches: HashMap::new(),
        corners: Vec::new(),
    };
    let mut t = Tile {
        id: 0,
        data: [[false; 10]; 10],
    };
    let mut i = 0;
    for line in read_lines(_input) {
        let l = line?;
        let lt = l.trim();
        if lt.len() == 0 {
            res.tiles.insert(t.id, t);
            t = Tile {
                id: 0,
                data: [[false; 10]; 10],
            };
            i = 0;
            continue;
        }
        if lt.starts_with("Tile") {
            t.id = lt.split(" ").nth(1).unwrap().trim_matches(':').parse()?;
        }
        else {
            for (j, v) in lt.chars().map(|c| c == '#').enumerate() {
                t.data[i][j] = v;
            }
            i += 1;
        }
    }
    res.tiles.insert(t.id, t);
    Ok(res)   
}

impl Problem for Day20{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        for &t in self.tiles.values() {
            let edges = [
                t.data[0],
                t.data[9],
                t.data.map(|row|row[0]),
                t.data.map(|row|row[9])
            ];
            for edge in edges.iter() {
                //println!("{}: edge {:?}, {}, {}", t.id, edge, to_u64(edge), to_u64_rev(edge));
                self.matches.entry(to_u64(edge)).or_insert(Vec::new()).push(t.id);
                self.matches.entry(to_u64_rev(edge)).or_insert(Vec::new()).push(t.id);
            }
        }
        //println!("{:#?}", self.matches);
        let mut uniques: HashMap<u64, u64> = HashMap::new(); // id -> number of unique borders (*2 for flips)
        for ids in self.matches.values() {
            if ids.len() == 1 {
                *uniques.entry(ids[0]).or_insert(0) += 1;
            }
        }
        //println!("{:#?}", uniques);
        let mut total = 1;
        for (id, count) in uniques.iter() {
            if *count == 4 {
                total *= id;
                self.corners.push(*id);
            }
        }
        Ok(total.to_string())
    }

    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut map = self.stitch_tiles();
        
        for i in 1..9 {
            if let Some(answer) = search(&map) {
                return Ok(answer.to_string());
            }
            map = rotate_vec(&map);
            if i == 4 {
                map = map.drain(0..).rev().collect();
            }
        }
        Err(new_err("didn't find any sea monsters"))
    }
}
