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
use std::collections::{HashSet, HashMap, VecDeque};

#[derive(Debug)]
pub struct Day22{
    boss_stats: Stats,
}

#[derive(Debug, Clone, Copy)]
struct Stats{
    hp: i64,
    damage: i64,
    armor: i64,
}

#[derive(Debug, Clone, Copy)]
enum SpellType{
    MagicMissile,
    Drain,
    Shield,
    Poison,
    Recharge,
}
fn cost(t: SpellType) -> i64 {
    match t{
        SpellType::MagicMissile => 53,
        SpellType::Drain => 73,
        SpellType::Shield => 113,
        SpellType::Poison => 173,
        SpellType::Recharge => 229,
    }
}
const ALL_SPELLS: [SpellType; 5] = [
    SpellType::MagicMissile,
    SpellType::Drain,
    SpellType::Shield,
    SpellType::Poison,
    SpellType::Recharge,
];

#[derive(Debug, Clone, Copy)]
struct State{
    boss_stats: Stats,
    player_stats: Stats,
    player_mana: i64,
    used_mana: i64,
    shield_turns: i64,
    poison_turns: i64,
    recharge_turns: i64,
}

fn proc_auras(state: &mut State) {
    if state.shield_turns > 0 {
        state.shield_turns -= 1;
    }
    if state.shield_turns == 0 {
        state.player_stats.armor = 0;
    }
    if state.poison_turns > 0 {
        state.poison_turns -= 1;
        state.boss_stats.hp -= 3;
    }
    if state.recharge_turns > 0 {
        state.recharge_turns -= 1;
        state.player_mana += 101;
    }
}

fn boss_turn(state: &mut State) {
    proc_auras(state);
    if state.boss_stats.hp <= 0 {
        return;
    }
    state.player_stats.hp -= hit_calc(state.boss_stats.damage, state.player_stats.armor);
}

fn cast(state: &State, spell: SpellType, hard_mode: bool) -> Option<State> {
    let mut new_state = state.clone();
    if hard_mode {
        new_state.player_stats.hp -= 1;
        if new_state.player_stats.hp <= 0 {
            return None;
        }
    }
    proc_auras(&mut new_state);
    if cost(spell) > new_state.player_mana {
        return None;
    }
    new_state.used_mana += cost(spell);
    new_state.player_mana -= cost(spell);
    
    match spell {
        SpellType::MagicMissile => new_state.boss_stats.hp -= 4,
        SpellType::Drain => {
            new_state.boss_stats.hp -= 2;
            new_state.player_stats.hp += 2;
        },
        SpellType::Shield => if new_state.shield_turns > 0 {
            return None;
        }
        else {
            new_state.player_stats.armor = 7;
            new_state.shield_turns = 6;
        },
        SpellType::Poison => if new_state.poison_turns > 0 {
            return None;
        }
        else {
            new_state.poison_turns = 6;
        },
        SpellType::Recharge => if new_state.recharge_turns > 0 {
            return None;
        }
        else {
            new_state.recharge_turns = 5;
        }
    }
    Some(new_state)
}

pub fn setup(_input:&str) -> Result<Day22, Box<dyn Error>>{
    let mut lines = read_lines(_input);
    let hp = lines.next().unwrap()?.split(" ").last().unwrap().parse::<i64>()?;
    let damage = lines.next().unwrap()?.split(" ").last().unwrap().parse::<i64>()?;
    Ok(Day22{
        boss_stats: Stats{hp, damage, armor: 0},
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

fn run(boss_stats: Stats, hard_mode: bool) -> i64 {
    let initial = State{
        player_stats: Stats{
            hp: 50,
            damage: 0,
            armor: 0
        },
        boss_stats: boss_stats,
        player_mana: 500,
        used_mana: 0,
        shield_turns: 0,
        poison_turns: 0,
        recharge_turns: 0,
    };
    let mut to_check: VecDeque<State> = VecDeque::new();
    to_check.push_back(initial);
    let mut min_mana_used = i64::MAX;
    while !to_check.is_empty() {
        let curr_state = to_check.pop_front().unwrap();
        for st in ALL_SPELLS {
            if let Some(mut new_state) = cast(&curr_state, st, hard_mode) {
                boss_turn(&mut new_state);
                if new_state.player_stats.hp <= 0 || new_state.used_mana >= min_mana_used {
                    continue;
                }
                else if new_state.boss_stats.hp <= 0 {
                    min_mana_used = new_state.used_mana;
                }
                else {
                    to_check.push_back(new_state);
                }
            }
        }
    }
    min_mana_used
}

impl Problem for Day22{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(run(self.boss_stats, false).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(run(self.boss_stats, true).to_string())
    }
}