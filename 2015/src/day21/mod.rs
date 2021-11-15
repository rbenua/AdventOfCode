use crate::Problem;
use crate::read_lines;
use std::error::Error;
use std::fs::read;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use json::*;
use std::result::Result;
use std::collections::{HashSet, HashMap};

#[derive(Debug)]
pub struct Day21{
    boss_stats: Stats,
    shop_weapons: Vec<Item>,
    shop_armor: Vec<Item>,
    shop_rings: Vec<Item>,
}

#[derive(Debug)]
struct Stats{
    hp: i64,
    damage: i64,
    armor: i64,
}
impl Stats{
    fn equip(&self, i: &Item) -> Stats{
        Stats{
            hp: self.hp,
            damage: self.damage + i.damage,
            armor: self.armor + i.armor,
        }
    }
}

#[derive(Debug)]
struct Item{
    name: &'static str,
    cost: i64,
    damage: i64,
    armor: i64,
}
impl From<(&'static str, i64, i64, i64)> for Item{
    fn from(t:(&'static str, i64, i64, i64)) -> Item{
        let (name, cost, damage, armor) = t;
        Item{name, cost, damage, armor}
    }
}

pub fn setup(_input:&str) -> Result<Day21, Box<dyn Error>>{
    let mut lines = read_lines(_input);
    let hp = lines.next().unwrap()?.split(" ").last().unwrap().parse::<i64>()?;
    let damage = lines.next().unwrap()?.split(" ").last().unwrap().parse::<i64>()?;
    let armor = lines.next().unwrap()?.split(" ").last().unwrap().parse::<i64>()?;
    let shop_weapons: Vec<Item> = vec![
    Item::from(("Dagger",        8,     4,       0)),
    Item::from(("Shortsword",   10,     5,       0)),
    Item::from(("Warhammer",    25,     6,       0)),
    Item::from(("Longsword",    40,     7,       0)),
    Item::from(("Greataxe",     74,     8,       0)),
    ];

    let shop_armor: Vec<Item> = vec![
    Item::from(("No armor",     0,      0,       0)),
    Item::from(("Leather",      13,     0,       1)),
    Item::from(("Chainmail",    31,     0,       2)),
    Item::from(("Splintmail",   53,     0,       3)),
    Item::from(("Bandedmail",   75,     0,       4)),
    Item::from(("Platemail",   102,     0,       5)),
    ];

    let shop_rings: Vec<Item> = vec![
    Item::from(("No ring 1",     0,      0,       0)),
    Item::from(("No ring 2",     0,      0,       0)),
    Item::from(("Damage +1",    25,     1,       0)),
    Item::from(("Damage +2",    50,     2,       0)),
    Item::from(("Damage +3",   100,     3,       0)),
    Item::from(("Defense +1",   20,     0,       1)),
    Item::from(("Defense +2",   40,     0,       2)),
    Item::from(("Defense +3",   80,     0,       3)),
    ];
    Ok(Day21{
        boss_stats: Stats{hp, damage, armor},
        shop_weapons, shop_armor, shop_rings
    })
}

fn hit_calc(damage: i64, armor: i64) -> i64 {
    let res = damage - armor;
    if res < 1 {
        1
    }
    else {
        res
    }
}

fn player_wins(player: &Stats, boss: &Stats) -> bool {
    let player_damage = hit_calc(player.damage, boss.armor);
    let boss_damage = hit_calc(boss.damage, player.armor);
    let player_ttk = ((boss.hp - 1) / player_damage) + 1;
    let boss_ttk = ((player.hp - 1) / boss_damage) + 1;
    player_ttk <= boss_ttk
}

impl Problem for Day21{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let player_base = Stats{
            hp: 100,
            damage: 0,
            armor: 0,
        };
        let mut min_cost: i64 = i64::MAX;
        for weapon in &self.shop_weapons {
            let player_weapon = player_base.equip(weapon);
            for armor in &self.shop_armor {
                let player_weap_armor = player_weapon.equip(armor);
                for i in 0..self.shop_rings.len() - 1 {
                    let player_ring1 = player_weap_armor.equip(&self.shop_rings[i]);
                    for j in i+1..self.shop_rings.len() {
                        let player_final = player_ring1.equip(&self.shop_rings[j]);
                        if player_wins(&player_final, &self.boss_stats) {
                            let cost = weapon.cost + armor.cost + self.shop_rings[i].cost + self.shop_rings[j].cost;
                            if cost < min_cost {
                                min_cost = cost;
                            }
                        }
                    }
                }
            }
        }
        Ok(min_cost.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let player_base = Stats{
            hp: 100,
            damage: 0,
            armor: 0,
        };
        let mut max_cost: i64 = 0;
        for weapon in &self.shop_weapons {
            let player_weapon = player_base.equip(weapon);
            for armor in &self.shop_armor {
                let player_weap_armor = player_weapon.equip(armor);
                for i in 0..self.shop_rings.len() - 1 {
                    let player_ring1 = player_weap_armor.equip(&self.shop_rings[i]);
                    for j in i+1..self.shop_rings.len() {
                        let player_final = player_ring1.equip(&self.shop_rings[j]);
                        if !player_wins(&player_final, &self.boss_stats) {
                            let cost = weapon.cost + armor.cost + self.shop_rings[i].cost + self.shop_rings[j].cost;
                            if cost > max_cost {
                                max_cost = cost;
                            }
                        }
                    }
                }
            }
        }
        Ok(max_cost.to_string())
    }
}