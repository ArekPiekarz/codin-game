#![allow(non_snake_case)]

use std::io;

macro_rules! parse_input {
    ($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}

fn main() {

    // game loop
    let mut currentCommand = 0;
    loop {
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        eprintln!("{}", input_line);
        let action_count = parse_input!(input_line, i32); // the number of spells and recipes in play
        let mut actions = vec![];
        for _ in 0..action_count as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            eprintln!("{}", input_line);
            let inputs = input_line.split(" ").collect::<Vec<_>>();
            let action_id = parse_input!(inputs[0], i32); // the unique ID of this spell or recipe
            let action_type = inputs[1].trim().to_string(); // in the first league: BREW; later: CAST, OPPONENT_CAST, LEARN, BREW
            if action_type == "OPPONENT_CAST" {
                continue;
            }
            let delta_0 = parse_input!(inputs[2], i32); // tier-0 ingredient change
            let delta_1 = parse_input!(inputs[3], i32); // tier-1 ingredient change
            let delta_2 = parse_input!(inputs[4], i32); // tier-2 ingredient change
            let delta_3 = parse_input!(inputs[5], i32); // tier-3 ingredient change
            let price = parse_input!(inputs[6], i32); // the price in rupees if this is a potion
            let _tome_index = parse_input!(inputs[7], i32); // in the first two leagues: always 0; later: the index in the tome if this is a tome spell, equal to the read-ahead tax; For brews, this is the value of the current urgency bonus
            let _tax_count = parse_input!(inputs[8], i32); // in the first two leagues: always 0; later: the amount of taxed tier-0 ingredients you gain from learning this spell; For brews, this is how many times you can still gain an urgency bonus
            let castable = parse_input!(inputs[9], i32); // in the first league: always 0; later: 1 if this is a castable player spell
            let _repeatable = parse_input!(inputs[10], i32); // for the first two leagues: always 0; later: 1 if this is a repeatable player spell

            actions.push(Action{id: action_id, kind: action_type, deltas: [delta_0, delta_1, delta_2, delta_3], price, castable: if castable == 1 { true} else {false}});
        }
        eprintln!("Actions: {}", actions.len());
        for (index, action) in actions.iter().enumerate() {
            eprintln!("{}) action: {:?}", index, action);
        }

        let mut inventory = Inventory::new();
        for i in 0..2 as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            eprintln!("{}", input_line);
            if i != 0 {
                continue;
            }
            let inputs = input_line.split(" ").collect::<Vec<_>>();
            let inv_0 = parse_input!(inputs[0], i32); // tier-0 ingredients in inventory
            let inv_1 = parse_input!(inputs[1], i32);
            let inv_2 = parse_input!(inputs[2], i32);
            let inv_3 = parse_input!(inputs[3], i32);
            let score = parse_input!(inputs[4], i32); // amount of rupees

            inventory.ingredients = [inv_0, inv_1, inv_2, inv_3];
            inventory.score = score;
        }
        eprintln!("inventory: {:?}", inventory);

        // let actionOpt = findRecipeWithEnoughIngredients(&actions, &inventory);
        // // eprintln!("after findRecipeWithEnoughIngredients: {:?}", actionOpt);
        // match actionOpt {
        //     Some(action) => println!("BREW {}", action.id),
        //     None => {
        //         let spellOpt = findSpellForMissingIngredient(&actions, &inventory);
        //         match spellOpt {
        //             Some(spell) => println!("CAST {}", spell.id),
        //             None => {
        //                 if isAnySpellExhausted(&actions) {
        //                     println!("REST");
        //                 } else {
        //                     println!("WAIT");
        //                 }
        //             }
        //         }
        //     }
        // }
        let commands = [
            Command::Cast([-1, 1, 0, 0]),
            Command::Cast([0, -1, 1, 0]),
            Command::Rest,
            Command::Cast([-1, 1, 0, 0]),
            Command::Cast([0, -1, 1, 0]),
            Command::Cast([2, 0, 0, 0]),
            Command::Brew(45)
        ];
        if currentCommand >= commands.len() {
            println!("WAIT");
        } else {
            match commands[currentCommand] {
                Command::Cast(deltas) => {
                    let spellIdOpt = findSpellIdWithDeltas(&deltas, &actions);
                    match spellIdOpt {
                        Some(id) => println!("CAST {}", id),
                        None => println!("WAIT")
                    }
                },
                Command::Rest => println!("REST"),
                Command::Brew(id) => println!("BREW {}", id)
            }
            currentCommand += 1;
        }
    }
}

#[derive(Debug)]
struct Action
{
    id: i32,
    kind: String,
    deltas: [i32; 4],
    price: i32,
    castable: bool
}

#[derive(Debug)]
struct Inventory
{
    ingredients: [i32; 4],
    score: i32
}

impl Inventory
{
    fn new() -> Self
    {
        Self{ingredients: [0, 0, 0 ,0], score: 0}
    }
}

enum Command
{
    Cast([i32; 4]),
    Rest,
    Brew(i32)
}

fn findSpellIdWithDeltas(deltas: &[i32; 4], actions: &[Action]) -> Option<i32>
{
    for action in actions {
        if action.kind != "CAST" {
            continue;
        }
        let mut valid = true;
        for i in 0..4 {
            if action.deltas[i] != deltas[i] {
                valid = false;
                break;
            }
        }
        if valid {
            return Some(action.id);
        }
    }
    None
}

fn findRecipeWithEnoughIngredients<'a>(actions: &'a [Action], inventory: &Inventory) -> Option<&'a Action>
{
    for action in actions {
        if action.kind != "BREW" {
            continue;
        }
        let mut valid = true;
        for i in 0..4 {
            // eprintln!("FIND i: {}, action.deltas[i].abs(): {}, inventory.ingredients[i]: {}", i, action.deltas[i].abs(), inventory.ingredients[i]);
            if action.deltas[i].abs() > inventory.ingredients[i] {
                // eprintln!("FIND i: {}, break", i);
                valid = false;
                break;
            }
        }
        if valid {
            return Some(action);
        }
    }
    None
}

fn findSpellForMissingIngredient<'a>(actions: &'a [Action], inventory: &Inventory) -> Option<&'a Action>
{
    let recipeOpt = findFirstRecipe(actions);
    // eprintln!("findSpellForMissingIngredient, first recipe: {:?}", recipeOpt);
    match recipeOpt {
        Some(recipe) => findSpellForMissingIngredientInRecipe(recipe, actions, inventory),
        None => None
    }
}

fn findFirstRecipe(actions: &[Action]) -> Option<&Action>
{
    for action in actions {
        if action.kind == "BREW" {
            return Some(action);
        }
    }
    None
}

fn findSpellForMissingIngredientInRecipe<'a>(recipe: &Action, actions: &'a [Action], inventory: &Inventory) -> Option<&'a Action>
{
    // eprintln!("findSpellForMissingIngredientInRecipe");
    for i in 0..4 {
        if recipe.deltas[i].abs() > inventory.ingredients[i] {
            return findSpellCreatingIngredient(i, actions, inventory);
        }
    }
    None
}

fn findSpellCreatingIngredient<'a>(i: usize, actions: &'a [Action], inventory: &Inventory) -> Option<&'a Action>
{
    // eprintln!("findSpellCreatingIngredient i: {}", i);
    for action in actions {
        if action.kind != "CAST" {
            continue;
        }
        if action.deltas[i] > 0 && action.castable {
            let result = doesInventoryAllowCastingSpell(action, inventory);
            match result {
                CastPermission::Allowed => return Some(action),
                CastPermission::NotAllowed(missingIngredient) => {
                    if missingIngredient != i {
                        return findSpellCreatingIngredient(missingIngredient, actions, inventory);
                    } else {
                        // return findSpellCreatingIngredient((missingIngredient + 1) % 4, actions, inventory);
                        // return findAnyCastableSpell(actions, inventory);
                        return None;
                    }
                }
                // CastNotAllowed(missingIngredient) => return findOtherSpellToAllowCastingCurrentSpell(action, missingIngredient, actions);
            }
        }
    }
    None
}

fn doesInventoryAllowCastingSpell(action: &Action, inventory: &Inventory) -> CastPermission
{
    for i in 0..4 {
        if action.deltas[i].abs() > inventory.ingredients[i] {
            return CastPermission::NotAllowed(i)
        }
    }
    CastPermission::Allowed
}

#[derive(Eq, PartialEq)]
enum CastPermission
{
    Allowed,
    NotAllowed(usize)
}

// fn findAnyCastableSpell<'a>(actions: &'a [Action], inventory: &Inventory) -> Option<&'a Action>
// {
//     for action in actions {
//         if action.kind != "CAST" {
//             continue;
//         }
//         if action.castable && doesInventoryAllowCastingSpell(action, inventory) == CastPermission::Allowed {
//             return Some(action);
//         }
//     }
//     None
// }

// fn findOtherSpellToAllowCastingCurrentSpell<'a>(action: &Action, missingIngredient: i32, actions: &'a [Action]) -> Option<&'a Action>
// {
// }

fn isAnySpellExhausted(actions: &[Action]) -> bool
{
    for action in actions {
        if action.kind == "CAST" && !action.castable {
            return true;
        }
    }
    false
}
