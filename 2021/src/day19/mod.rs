use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::{HashSet, VecDeque};

#[derive(Debug, PartialOrd, Ord, Eq, PartialEq, Hash, Copy, Clone)]
struct Mat3{
    el: [[i32; 3]; 3],
}
impl Mat3 {
    fn mul(&self, r: &Mat3) -> Mat3 {
        let mut res = Mat3{
            el: [[0;3];3]
        };
        for i in 0..3 {
            for j in 0..3 {
                res.el[i][j] = self.el[i][0] * r.el[0][j] + self.el[i][1] * r.el[1][j] + self.el[i][2] * r.el[2][j];
            }
        }
        res
    }
    fn mulv(&self, r: &Vec3) -> Vec3 {
        let mut res = Vec3 {
            el: [0; 3],
        };
        for i in 0..3 {
            res.el[i] = self.el[i][0] * r.el[0] + self.el[i][1] * r.el[1] + self.el[i][2] * r.el[2];
        }
        res
    }
}
impl std::fmt::Display for Mat3{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.el)
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
struct Vec3{
    el: [i32; 3],
}

impl std::fmt::Display for Vec3 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.el)
    }
}
impl std::fmt::Debug for Vec3 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.el)
    }
}

impl std::ops::Add for Vec3 {
    type Output = Self;
    fn add(self, rhs: Vec3) -> Vec3 {
        Vec3{
            el: [self.el[0] + rhs.el[0], self.el[1] + rhs.el[1], self.el[2] + rhs.el[2]],
        }
    }
}
impl std::ops::Sub for Vec3 {
    type Output = Self;
    fn sub(self, rhs: Vec3) -> Vec3 {
        Vec3{
            el: [self.el[0] - rhs.el[0], self.el[1] - rhs.el[1], self.el[2] - rhs.el[2]],
        }
    }
}

impl Vec3 {
    fn mdist(&self, other: &Vec3) -> i32 {
          (self.el[0] - other.el[0]).abs()
        + (self.el[1] - other.el[1]).abs()
        + (self.el[2] - other.el[2]).abs()
    }
}

const ID: Mat3 = Mat3 {
    el: [[1, 0, 0],
         [0, 1, 0],
         [0, 0, 1]],
};
const X_ROT: Mat3 = Mat3 {
    el: [[1, 0, 0],
         [0, 0, -1],
         [0, 1, 0]],
};
const Y_ROT: Mat3 = Mat3 {
    el: [[0, 0, 1],
         [0, 1, 0],
         [-1, 0, 0]],
};
const Z_ROT: Mat3 = Mat3 {
    el: [[0, -1, 0],
         [1, 0, 0],
         [0, 0, 1]],
};

fn compute_rotations() -> Vec<Mat3> {
    let mut res = Vec::new();
    let mut up = ID;
    for _ in 0..4 {
        let mut fwd = ID;
        for _ in 0..4 {
            let dir = up.mul(&fwd);
            res.push(dir);
            fwd = fwd.mul(&Y_ROT);
        }
        fwd = fwd.mul(&Z_ROT);
        let dir = up.mul(&fwd);
        res.push(dir);
        fwd = fwd.mul(&Z_ROT);
        fwd = fwd.mul(&Z_ROT);
        let dir = up.mul(&fwd);
        res.push(dir);

        up = up.mul(&X_ROT);
    }
    res
}

pub struct Day19{
    scanners: Vec<Scanner>,
    transforms: Vec<Mat3>,
    aligned: Option<Vec<Scanner>>,
}

#[derive(Debug, Clone)]
struct Scanner {
    id: i64,
    offset: Option<Vec3>,
    pts: HashSet<Vec3>,
}

pub fn setup(_input:&str) -> Result<Day19, Box<dyn Error>>{
    let mut res = Day19{
        scanners: Vec::new(),
        transforms: compute_rotations(),
        aligned: None,
    };
    let re = Regex::new(r"--- scanner (\d+) ---")?;
    let mut it = read_lines(_input);
    let first_line = it.next().unwrap().unwrap();
    let first_cs = re.captures(&first_line).unwrap();
    let mut curr_id: i64 = first_cs[1].parse()?;
    let mut curr_pts = HashSet::new();
    for line_opt in it {
        let line = line_opt?;
        if let Some(cs) = re.captures(&line) {
            res.scanners.push(Scanner {
                id: curr_id,
                offset: None,
                pts: curr_pts,
            });
            curr_pts = HashSet::new();
            curr_id = cs[1].parse()?;
        }
        else if line.len() > 0 {
            let mut sp = line.split(',').map(|s|s.parse::<i32>().unwrap());
            curr_pts.insert(Vec3{
                el: [sp.next().unwrap(), sp.next().unwrap(), sp.next().unwrap()],
            });
        }
    }
    res.scanners.push(Scanner {
        id: curr_id,
        offset: None,
        pts: curr_pts,
    });
    Ok(res)
}

impl Day19 {
    fn matches(&self, existing: &Scanner, new: &Scanner) -> Option<Scanner> {
        print!("Matches({:?}, {:?})... ", existing.id, new.id);
        //println!("existing points: {:#?}", existing.pts);
        let mut max_overlap = 0;
        //let mut max_pts = HashSet::new();
        for transform in &self.transforms {
            //println!("Checking scanner {} against {} * scanner {}", existing.id, transform, new.id);
            let mut pts = new.pts.iter().map(|p|transform.mulv(p)).collect::<Vec<Vec3>>();
            //println!("new points: {:#?}", pts);
            for p1 in &existing.pts {
                for p2 in &pts {
                    let offset = *p1 - *p2;
                    //println!("{} - {} = {}", p1, p2, offset);
                    let mut mapped_vec = pts.iter().map(|p|*p + offset).collect::<Vec<Vec3>>();
                    //println!("offset points: {:#?}", mapped_vec);
                    let mapped = mapped_vec.drain(..).collect();
                    let count = existing.pts.intersection(&mapped).count();
                    //println!("overlap: {}", count);
                    if count > max_overlap {
                        max_overlap = count;
                        /*
                        max_pts = existing.pts.intersection(&mapped).cloned().collect();
                        println!("new max overlap {}", max_overlap);
                        println!("existing: {:#?}", existing.pts);
                        println!("transformed: {:#?}", mapped);
                        println!("overlap: {:#?}", max_pts);
                        */
                    }
                    if count >= 12 {
                        return Some(Scanner{
                            id: new.id,
                            offset: Some(offset),
                            pts: mapped,
                        });
                    }
                }
            }
        }
        println!("Not found, max overlap: {}", max_overlap);
        None
    }

    fn build_aligned_list(&self) -> Vec<Scanner> {
        let mut res: Vec<Scanner> = Vec::new();
        let mut remaining = self.scanners.iter().cloned().collect::<VecDeque<Scanner>>();
        let mut first = remaining.pop_front().unwrap();
        first.offset = Some(Vec3{el:[0,0,0]});
        res.push(first);
        while !remaining.is_empty() {
            let to_check = remaining.pop_front().unwrap();
            let mut found = false;
            for c in &res {
                if let Some(aligned) = self.matches(c, &to_check) {
                    println!("Aligned scanner {} at offset {}", aligned.id, aligned.offset.unwrap());
                    res.push(aligned);
                    found = true;
                    break;
                }
            }
            if !found {
                remaining.push_back(to_check);
            }
        }
        res
    }
}

impl Problem for Day19{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let aligned = self.build_aligned_list();
        let size = aligned.iter().map(|s|s.pts.iter()).flatten().cloned().collect::<HashSet<Vec3>>().len();
        self.aligned = Some(aligned);
        Ok(size.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let aligned = self.aligned.as_ref().unwrap();
        let mut max = 0;
        for i in 0..(aligned.len() - 1) {
            let cur = aligned[i].offset.unwrap();
            let best = aligned[(i+1)..].iter().map(|new|cur.mdist(&new.offset.unwrap())).max().unwrap();
            if best > max {
                max = best;
            }
        }
        Ok(max.to_string())
    }
}
