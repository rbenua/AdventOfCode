use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct Day21{
    all_ingredients: HashSet<String>,
    all_allergens: HashSet<String>,
    dishes: Vec<(HashSet<String>, HashSet<String>)>,
    possibles: HashMap<String, HashSet<String>>,
}

pub fn setup(_input:&str) -> Result<Day21, Box<dyn Error>>{
    let mut res = Day21{
        all_ingredients: HashSet::new(),
        all_allergens: HashSet::new(),
        dishes: Vec::new(),
        possibles: HashMap::new(),
    };
    let re = Regex::new(r"((?:\w+ )+)\(contains ((?:\w+,? ?)+)\)")?;
    for line in read_lines(_input) {
        let mut cur_ingredients = HashSet::new();
        let mut cur_allergens = HashSet::new();
        let ugh = line?;
        let caps = re.captures(&ugh).unwrap();
        for ingredient in caps[1].trim().split(" ") {
            res.all_ingredients.insert(ingredient.to_string());
            cur_ingredients.insert(ingredient.to_string());
        }
        for allergen in caps[2].split(",") {
            res.all_allergens.insert(allergen.trim().to_string());
            cur_allergens.insert(allergen.trim().to_string());
        }
        res.dishes.push((cur_ingredients, cur_allergens));
    }
    //println!("{:#?}", res);
    Ok(res)   
}

impl Problem for Day21{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        for (ingredients, allergens) in &self.dishes {
            for allergen in allergens {
                self.possibles.insert(allergen.to_string(), match self.possibles.get(allergen) {
                    None => ingredients,
                    Some(s) => s,
                }.intersection(ingredients).cloned().collect());
            }
        }

        let mut impossibles = self.all_ingredients.clone();
        for v in self.possibles.values() {
            for ing in v {
                impossibles.remove(ing);
            }
        }

        let mut count = 0;
        for (ingredients, _) in &self.dishes {
            count += ingredients.intersection(&impossibles).count();
        }

        //println!("{:#?}", possibles);
        //println!("{:#?}", impossibles);
        Ok(count.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        
        let mut poss_vec : Vec<(String, HashSet<String>)> = self.possibles.iter().map(|(k,v)|(k.clone(),v.clone())).collect();
        let size = poss_vec.len();
        while poss_vec.iter().map(|(_,v)|v.len()).max() > Some(1) {
            for i in 0..size {
                let (_, v) = &poss_vec[i];
                if v.len() == 1 {
                    let name: String = v.iter().next().unwrap().clone();
                    for j in 0..size {
                        if i != j {
                            poss_vec[j].1.remove(&name);
                        }
                    }
                }
            }
        }
        poss_vec.sort_by(|(k1,_),(k2,_)|k1.cmp(k2));
        let names: Vec<&str> = poss_vec.iter().map(|(_,v)|v.iter().next().unwrap().as_str()).collect();
        Ok(names.join(",").to_string())
    }
}
