#![allow(non_snake_case)]

use std::io;
use std::convert::TryFrom;

macro_rules! parse_input {
    ($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}


fn main() {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let size = parse_input!(input_line, i32);
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let units_per_player = parse_input!(input_line, i32);

    loop {
        let mut area = vec![];
        for _ in 0..size as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            let row = input_line.trim().to_string();
            area.push(row);
        }
        let mut playerUnits = vec![];
        for _ in 0..units_per_player as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            let inputs = input_line.split(" ").collect::<Vec<_>>();
            let unitX = parse_input!(inputs[0], i32);
            let unitY = parse_input!(inputs[1], i32);
            playerUnits.push(Unit::new(unitY, unitX));
        }
        for _ in 0..units_per_player as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            let inputs = input_line.split(" ").collect::<Vec<_>>();
            let _other_x = parse_input!(inputs[0], i32);
            let _other_y = parse_input!(inputs[1], i32);
        }
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let legalActionsSize = parse_input!(input_line, i32);
        let mut legalActions = vec![];
        for _ in 0..legalActionsSize as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            let inputs = input_line.split(" ").collect::<Vec<_>>();
            let atype = inputs[0].trim().to_string();
            let index = parse_input!(inputs[1], i32);
            let dir1 = inputs[2].trim().to_string();
            let dir2 = inputs[3].trim().to_string();
            legalActions.push(Action::new(atype, index, Direction::from(&dir1), Direction::from(&dir2)));
        }

        for (index, action) in legalActions.iter().enumerate() {
            eprintln!("AAA action {}: {:?}", index, action);
        }

        if let Some(action) = chooseAction(&legalActions, &area, &playerUnits[0]) {
            println!("{} {} {} {}", action.kind, action.index, action.moveDirection.toStr(), action.buildDirection.toStr());
        }
    }
}

fn chooseAction<'a>(legalActions: &'a [Action], area: &Area, unit: &Unit) -> Option<&'a Action>
{
    if legalActions.is_empty() {
        return None;
    }

    for action in legalActions {
        if actionWouldMoveToHigherLevel(action, area, unit) {
            return Some(action);
        }
    }

    for action in legalActions {
        if actionWouldMoveToSameLevel(action, area, unit) {
            return Some(action);
        }
    }

    for action in legalActions {
        if actionWouldMoveToLowerByOneLevel(action, area, unit) {
            return Some(action);
        }
    }

    for action in legalActions {
        if actionWouldMoveToLowerByTwoLevel(action, area, unit) {
            return Some(action);
        }
    }

    let actionIndex = rand::random::<usize>() % legalActions.len();
    Some(&legalActions[actionIndex])
}

fn actionWouldMoveToHigherLevel(action: &Action, area: &Area, unit: &Unit) -> bool
{
    // let currentPos = &unit.pos;
    // let newPos = calculatePosition(currentPos, action.moveDirection);
    // let currentLevel = getLevel(currentPos, area);
    // let newLevel = getLevel(&newPos, area);
    let (currentLevel, newLevel) = calculateCurrentAndNewLevels(action, area, unit);
    currentLevel < newLevel
}

fn actionWouldMoveToSameLevel(action: &Action, area: &Area, unit: &Unit) -> bool
{
    let (currentLevel, newLevel) = calculateCurrentAndNewLevels(action, area, unit);
    currentLevel == newLevel
}

fn actionWouldMoveToLowerByOneLevel(action: &Action, area: &Area, unit: &Unit) -> bool
{
    let (currentLevel, newLevel) = calculateCurrentAndNewLevels(action, area, unit);
    currentLevel == newLevel + 1
}

fn actionWouldMoveToLowerByTwoLevel(action: &Action, area: &Area, unit: &Unit) -> bool
{
    let (currentLevel, newLevel) = calculateCurrentAndNewLevels(action, area, unit);
    currentLevel == newLevel + 2
}

fn calculateCurrentAndNewLevels(action: &Action, area: &Area, unit: &Unit) -> (CurrentLevel, NewLevel)
{
    let currentPos = &unit.pos;
    let newPos = calculatePosition(currentPos, action.moveDirection);
    let currentLevel = getLevel(currentPos, area);
    let newLevel = getLevel(&newPos, area);
    (currentLevel, newLevel)
}

type CurrentLevel = Level;
type NewLevel = Level;

fn getLevel(pos: &Position, area: &Area) -> Level
{
    let row = usize::try_from(pos.row).unwrap();
    let col = usize::try_from(pos.col).unwrap();
    let level = area[row].chars().nth(col).unwrap().to_string().parse::<u8>().unwrap();
    level
}

fn calculatePosition(currentPos: &Position, direction: Direction) -> Position
{
    let mut newPos = currentPos.clone();
    match direction {
        Direction::N  => newPos.row -= 1,
        Direction::NE => { newPos.row -= 1; newPos.col += 1; },
        Direction::E  => newPos.col += 1,
        Direction::SE => { newPos.row += 1; newPos.col += 1; },
        Direction::S  => newPos.row += 1,
        Direction::SW => { newPos.row += 1; newPos.col -= 1; },
        Direction::W  => newPos.col -= 1,
        Direction::NW => { newPos.row -= 1; newPos.col -= 1; }
    }
    eprintln!("AAA calcPos, current: {:?}, dir: {:?}, new: {:?}", currentPos, direction, newPos);
    newPos
}

type Area = Vec<String>;
type Level = u8;

#[derive(Debug)]
struct Action
{
    kind: String,
    index: i32,
    moveDirection: Direction,
    buildDirection: Direction
}

impl Action
{
    fn new(kind: String, index: i32, moveDirection: Direction, buildDirection: Direction) -> Self
    {
        Self{kind, index, moveDirection, buildDirection}
    }
}

#[derive(Clone, Copy, Debug)]
enum Direction
{
    N,
    NE,
    E,
    SE,
    S,
    SW,
    W,
    NW
}

impl Direction
{
    fn from(text: &str) -> Self
    {
        match text {
            "N"  => Self::N,
            "NE" => Self::NE,
            "E"  => Self::E,
            "SE" => Self::SE,
            "S"  => Self::S,
            "SW" => Self::SW,
            "W"  => Self::W,
            "NW" => Self::NW,
            _ => panic!("Unknown direction: {}", text)
        }
    }

    fn toStr(&self) -> &str
    {
        match self {
            Self::N  => "N",
            Self::NE => "NE",
            Self::E  => "E",
            Self::SE => "SE",
            Self::S  => "S",
            Self::SW => "SW",
            Self::W  => "W",
            Self::NW => "NW"
        }
    }
}

struct Unit
{
    pos: Position
}

impl Unit
{
    fn new(row: i32, col: i32) -> Self
    {
        Self{pos: Position{row, col}}
    }
}

#[derive(Clone, Debug)]
struct Position
{
    row: i32,
    col: i32
}
