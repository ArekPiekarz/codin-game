#![allow(non_snake_case)]

use std::cell::RefCell;
use std::ops::DerefMut;
use std::io::stdin;
use std::ops::Deref;
use std::rc::Rc;

macro_rules! parseInput {
    ($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}

fn main()
{
    let mut gameState = GameState::new();
    let mut actionPlan = vec![];
    loop {
        let mut inputLine = String::new();
        stdin().read_line(&mut inputLine).unwrap();
        // eprintln!("{}", inputLine);
        let actionCount = parseInput!(inputLine, i32);
        gameState.actionRecipes.clear();
        for _ in 0..actionCount as usize {
            let mut inputLine = String::new();
            stdin().read_line(&mut inputLine).unwrap();
            // eprintln!("{}", inputLine);
            let inputs = inputLine.split(" ").collect::<Vec<_>>();
            let actionId = parseInput!(inputs[0], u32);
            let actionType = inputs[1].trim().to_string();
            if actionType == "OPPONENT_CAST" {
                continue;
            }

            let delta0 = parseInput!(inputs[2], i32);
            let delta1 = parseInput!(inputs[3], i32);
            let delta2 = parseInput!(inputs[4], i32);
            let delta3 = parseInput!(inputs[5], i32);
            let price = parseInput!(inputs[6], i32);
            let readAheadTaxOrUrgencyBonus = parseInput!(inputs[7], i32);
            let _taxCountOrUrgencyBonusCount = parseInput!(inputs[8], i32);
            let castable = toBool(parseInput!(inputs[9], i32));
            let repeatable = toBool(parseInput!(inputs[10], i32));

            gameState.actionRecipes.push(ActionRecipe::new(
                actionId,
                &actionType,
                [delta0, delta1, delta2, delta3],
                price,
                readAheadTaxOrUrgencyBonus,
                castable,
                repeatable
            ));
        }
        gameState.actionRecipes.push(ActionRecipe::Rest);
        // gameState.actionRecipes.push(ActionRecipe::Wait);

        eprintln!("Actions recipes: {}", gameState.actionRecipes.len());
        for (index, actionRecipe) in gameState.actionRecipes.iter().enumerate() {
            eprintln!("{}) action recipe: {:?}", index, actionRecipe);
        }

        for i in 0..2 as usize {
            let mut input_line = String::new();
            stdin().read_line(&mut input_line).unwrap();
            // eprintln!("{}", input_line);
            if i != 0 {
                continue;
            }
            let inputs = input_line.split(" ").collect::<Vec<_>>();
            let ingredient0 = parseInput!(inputs[0], i32);
            let ingredient1 = parseInput!(inputs[1], i32);
            let ingredient2 = parseInput!(inputs[2], i32);
            let ingredient3 = parseInput!(inputs[3], i32);
            let score = parseInput!(inputs[4], i32);

            gameState.ingredients = [ingredient0, ingredient1, ingredient2, ingredient3];
            gameState.score = score;
        }

        // TODO check if actionPlan contains Tome Spells to learn which are not available anymore
        // TODO reset actionPlan when potion is brewed?
        if actionPlan.is_empty() || !canPerformAction(&actionPlan[0], &gameState) {
            eprintln!("AAA runMonteCarloTreeSearch");
            actionPlan = runMonteCarloTreeSearch(&gameState);
        }
        eprintln!("AAA actionPlan (size: {}): {:?}", actionPlan.len(), actionPlan);
        performAction(&mut actionPlan, &mut gameState);
    }
}

fn findCheapestSpellToLearn(actionRecipes: &[ActionRecipe]) -> ActionId
{
    let mut cheapestId = u32::MAX;
    let mut cheapestTax = u32::MAX;
    for actionRecipe in actionRecipes {
        match actionRecipe {
            ActionRecipe::Learn{id, ingredientsDelta: _, repeatable: _, readAheadTax} => {
                if *readAheadTax < cheapestTax {
                    cheapestId = *id;
                    cheapestTax = *readAheadTax;
                }
            },
            _ => ()
        }
    }
    cheapestId
}

fn toBool(value: i32) -> bool
{
    match value {
        0 => false,
        1 => true,
        _ => panic!("unknown bool value: {}", value)
    }
}

fn performAction(actionPlan: &mut ActionPlan, gameState: &mut GameState)
{
    match actionPlan[0] {
        Action::Brew{id, ..} => {
            println!("BREW {}", id);
            gameState.potionsBrewed += 1;
        },
        Action::Cast{id, ingredientsDelta: _, times} => println!("CAST {} {}", id, times),
        Action::Learn{id, ..} => println!("LEARN {}", id),
        Action::Rest => println!("REST"),
        Action::Wait => println!("WAIT")
    };
    actionPlan.remove(0);
    gameState.turns += 1;

}

type ActionPlan = Vec<Action>;

#[derive(Clone, Debug)]
enum ActionRecipe
{
    Brew{id: ActionId, ingredientsDelta: IngredientsDelta, price: Price},
    Cast{id: ActionId, ingredientsDelta: IngredientsDelta, castable: bool, repeatable: bool},
    Learn{id: ActionId, ingredientsDelta: IngredientsDelta, repeatable: bool, readAheadTax: u32},
    Rest
}

type ReadAheadTax = u32;

impl ActionRecipe
{
    fn new(
        id: ActionId,
        kind: &str,
        ingredientsDelta: IngredientsDelta,
        price: Price,
        readAheadTaxOrUrgencyBonus: i32,
        castable: bool,
        repeatable: bool)
        -> Self
    {
        match kind {
            "BREW" => Self::Brew{id, ingredientsDelta, price},
            "CAST" => Self::Cast{id, ingredientsDelta, castable, repeatable},
            "LEARN" => Self::Learn{
                id, ingredientsDelta, repeatable, readAheadTax: readAheadTaxOrUrgencyBonus as ReadAheadTax},
            _ => panic!("unexpected action recipe kind: {}", kind)
        }
    }
}

#[derive(Clone, Debug)]
enum Action
{
    Brew{id: ActionId, ingredientsDelta: IngredientsDelta, price: Price},
    Cast{id: ActionId, ingredientsDelta: IngredientsDelta, times: u32},
    Learn{id: ActionId, ingredientsDelta: IngredientsDelta, repeatable: bool, readAheadTax: ReadAheadTax},
    Rest,
    Wait
}

type ActionId = u32;
type Price = i32;
type Score = i32;

fn runMonteCarloTreeSearch(gameState: &GameState) -> Vec<Action>
{
    let mut tree = Tree::new();
    let rootNode = tree.getRoot();
    fillRootNode(&rootNode, &gameState);
    makeChildNodesFromAvailableActions(&rootNode, &mut tree);
    if rootNode.borrow().children.is_empty() {
        return vec![Action::Wait];
    }

    let mut currentNode = Rc::clone(&rootNode);
    for _ in 0..1000 {
        if currentNode.borrow().isLeaf() {
            if currentNode.borrow().visits == 0 {
                let score = rollout(&currentNode);
                backpropagate(&currentNode, score, &tree);
                currentNode = findBestChildNodeToExplore(&rootNode, &tree).unwrap();
            } else {
                makeChildNodesFromAvailableActions(&currentNode, &mut tree);
                if currentNode.borrow().children.is_empty() {
                    break;
                }
                let childNodeId = currentNode.borrow().children[0];
                currentNode = tree.getNode(childNodeId);
                let score = rollout(&currentNode);
                backpropagate(&currentNode, score, &tree);
                currentNode = findBestChildNodeToExplore(&rootNode, &tree).unwrap();
            }
        } else {
            match findBestChildNodeToExplore(&currentNode, &tree) {
                Some(node) => currentNode = node,
                None => break
            }
        }
    }

    #[cfg(feature = "visualize")]
    visualizeTree(&tree);
    findBestActionPlan(&tree)
}

fn fillRootNode(rootNode: &Rc<RefCell<Node>>, gameState: &GameState)
{
    let mut rootNode = rootNode.borrow_mut();
    rootNode.state.ingredients = gameState.ingredients;
    rootNode.state.actionRecipes = gameState.actionRecipes.clone();
    rootNode.state.score = gameState.score;
}

fn findBestChildNodeToExplore(parentNode: &Rc<RefCell<Node>>, tree: &Tree) -> Option<Rc<RefCell<Node>>>
{
    let mut bestNodeGradeOpt: Option<NodeGrade> = None;
    let parentNode = parentNode.borrow();
    let parentNodeVisits = parentNode.visits;
    for childNodeId in &parentNode.children {
        let childNode = tree.getNode(*childNodeId);
        let childNode = childNode.borrow();
        if childNode.visits == 0 {
            return Some(tree.getNode(*childNodeId));
        } else {
            let grade = (childNode.value as f32) + ((parentNodeVisits as f32).ln() / childNode.visits as f32).sqrt();
            match &bestNodeGradeOpt {
                Some(bestNodeGrade) => {
                    if grade > bestNodeGrade.grade {
                        bestNodeGradeOpt = Some(NodeGrade { grade, nodeId: *childNodeId })
                    }
                },
                None => bestNodeGradeOpt = Some(NodeGrade { grade, nodeId: *childNodeId })
            }
        }
    }
    match bestNodeGradeOpt {
        Some(NodeGrade{grade: _, nodeId}) => Some(tree.getNode(nodeId)),
        None => None
    }
}

fn findBestActionPlan(tree: &Tree) -> Vec<Action>
{
    let mut actionPlan = vec![];
    let mut currentNode = tree.getRoot();
    loop
    {
        let bestChildNode = findChildNodeWithHighestValue(&currentNode, tree);
        actionPlan.push(bestChildNode.borrow().action.clone());
        if bestChildNode.borrow().isLeaf() {
            break;
        }
        currentNode = bestChildNode;
    }
    actionPlan
}

fn findChildNodeWithHighestValue(parentNode: &Rc<RefCell<Node>>, tree: &Tree) -> Rc<RefCell<Node>>
{
    let mut bestNodeGradeOpt: Option<NodeGrade> = None;
    let parentNode = parentNode.borrow();
    for childNodeId in &parentNode.children {
        let childNode = tree.getNode(*childNodeId);
        let childNode = childNode.borrow();

        let grade = childNode.value;
        match &bestNodeGradeOpt {
            Some(bestNodeGrade) => {
                if grade as f32 > bestNodeGrade.grade {
                    bestNodeGradeOpt = Some(NodeGrade { grade: grade as f32, nodeId: *childNodeId })
                }
            },
            None => bestNodeGradeOpt = Some(NodeGrade { grade: grade as f32, nodeId: *childNodeId })
        }
    }

    tree.getNode(bestNodeGradeOpt.unwrap().nodeId)
}

fn rollout(currentNode: &Rc<RefCell<Node>>) -> Score
{
    let mut state = currentNode.borrow().state.clone();
    loop {
        let possibleActions = makePerformableActionsFromRecipes(&state);
        if isTerminalState(&state, &possibleActions) {
            return calculateStateScore(&state);
        }

        let action = chooseRandomAction(&possibleActions);
        state = calculateGameStateAfterAction(&state, action);
    }
}

fn isTerminalState(state: &GameState, possibleActions: &[Action]) -> bool
{
    possibleActions.is_empty() || state.turns >= MAX_TURNS || state.potionsBrewed >= MAX_POTIONS
}

const MAX_TURNS: u32 = 100;
const MAX_POTIONS: u32 = 6;

fn chooseRandomAction(actions: &[Action]) -> &Action
{
    &actions[rand::random::<usize>() % actions.len()]
}

fn calculateStateScore(state: &GameState) -> Score
{
    // TODO include potion's price and the state's current score in the output score
    let mut score: Score = (state.potionsBrewed as Score) * 100;
    for i in 0..state.ingredients.len() {
        // score += state.ingredients[i] * ((i as Score)+1);
        score += state.ingredients[i];
    }
    // score -= state.exhaustedSpells.len() as Score;
    // score -= state.turns as Score;
    score
}

fn backpropagate(currentNode: &Rc<RefCell<Node>>, score: Score, tree: &Tree)
{
    let mut currentNode = Rc::clone(currentNode);
    loop {
        currentNode.borrow_mut().value += score;
        currentNode.borrow_mut().visits += 1;
        let parent = currentNode.borrow_mut().parent;
        match parent {
            Some(parentNodeId) => currentNode = tree.getNode(parentNodeId),
            None => return
        }
    }
}

#[derive(Debug)]
struct NodeGrade
{
    grade: f32,
    nodeId: NodeId
}

#[derive(Debug)]
struct Tree
{
    nodes: Vec<Rc<RefCell<Node>>>
}

impl Tree
{
    fn new() -> Self
    {
        let nodeId = 0;
        Self{nodes: vec![Rc::new(RefCell::new(Node::new(nodeId, GameState::new(), NO_PARENT, Action::Wait)))]}
    }

    fn getRoot(&self) -> Rc<RefCell<Node>>
    {
        Rc::clone(&self.nodes[0])
    }

    fn getNode(&self, id: NodeId) -> Rc<RefCell<Node>>
    {
        Rc::clone(&self.nodes[id])
    }

    fn addNode(&mut self, state: GameState, parentNodeId: NodeId, action: Action) -> NodeId
    {
        let newNodeId = self.nodes.len();
        self.nodes.push(Rc::new(RefCell::new(Node::new(newNodeId, state, Some(parentNodeId), action))));
        newNodeId
    }
}

const NO_PARENT: Option<NodeId> = None;
type NodeId = usize;

#[derive(Clone, Debug)]
struct Node
{
    id: NodeId,
    state: GameState,
    parent: Option<NodeId>,
    action: Action,
    children: Vec<NodeId>,
    value: Score,
    visits: u32
}

impl Node
{
    fn new(id: NodeId, state: GameState, parent: Option<NodeId>, action: Action) -> Self
    {
        Self{id, state, parent, action, children: vec![], value: 0, visits: 0}
    }

    fn isLeaf(&self) -> bool
    {
        self.children.is_empty()
    }
}

type Ingredients = [i32;4];
type IngredientsDelta = [i32;4];

#[derive(Clone, Debug)]
struct GameState
{
    ingredients: Ingredients,
    actionRecipes: Vec<ActionRecipe>,
    score: i32,
    potionsBrewed: u32,
    turns: u32
}

impl GameState
{
    fn new() -> Self
    {
        Self{ingredients: [0, 0, 0, 0], actionRecipes: vec![], score: 0, potionsBrewed: 0, turns: 0}
    }
}

#[derive(Clone, Debug)]
struct ExhaustedSpell
{
    id: ActionId
}

impl ExhaustedSpell
{
    fn new(id: ActionId) -> Self
    {
        Self{id}
    }
}


fn makeChildNodesFromAvailableActions(currentNode: &Rc<RefCell<Node>>, tree: &mut Tree)
{
    let mut currentNode = currentNode.borrow_mut();
    let mut currentNode = currentNode.deref_mut();
    for actionRecipe in &currentNode.state.actionRecipes {
        let performableActions = makePerformableActionsFromRecipe(actionRecipe, &currentNode.state);
        for action in performableActions {
            let childNodeId = tree.addNode(calculateGameStateAfterAction(&currentNode.state, &action), currentNode.id, action);
            currentNode.children.push(childNodeId);
        }
    }
}

fn makePerformableActionsFromRecipes(gameState: &GameState) -> Vec<Action>
{
    let mut actions = vec![];
    for actionRecipe in &gameState.actionRecipes {
        actions.extend(makePerformableActionsFromRecipe(actionRecipe, gameState));
    }
    actions
}

fn makePerformableActionsFromRecipe(actionRecipe: &ActionRecipe, gameState: &GameState) -> Vec<Action>
{
    let mut actions = vec![];
    match actionRecipe {
        ActionRecipe::Brew{id, ingredientsDelta, price} => {
            pushIfCanPerformAction(Action::Brew{id: *id, ingredientsDelta: *ingredientsDelta, price: *price}, &mut actions, gameState);
        },
        ActionRecipe::Cast{id, ingredientsDelta, castable, repeatable} => {
            actions = makePerformableCastsFromRecipe(*id, ingredientsDelta, *castable, *repeatable, gameState)
        },
        ActionRecipe::Learn{id, ingredientsDelta, repeatable, readAheadTax} => {
            pushIfCanPerformAction(Action::Learn{
                id: *id, ingredientsDelta: *ingredientsDelta, repeatable: *repeatable, readAheadTax: *readAheadTax}, &mut actions, gameState);
        }
        ActionRecipe::Rest => {
            pushIfCanPerformAction(Action::Rest, &mut actions, gameState);
        }
    }
    actions
}

fn makePerformableCastsFromRecipe(id: ActionId, ingredientsDelta: &IngredientsDelta, castable: bool, repeatable: bool, gameState: &GameState) -> Vec<Action>
{
    let mut actions = vec![];
    if !castable {
        return actions;
    }

    if repeatable {
        loop {
            let action = Action::Cast{id, ingredientsDelta: *ingredientsDelta, times: (actions.len() as u32) + 1};
            if !pushIfCanPerformAction(action, &mut actions, gameState) {
                break;
            }
        }
    } else {
        pushIfCanPerformAction(Action::Cast{id, ingredientsDelta: *ingredientsDelta, times: 1}, &mut actions, gameState);
    }
    actions
}

fn pushIfCanPerformAction(action: Action, actions: &mut Vec<Action>, gameState: &GameState) -> bool
{
    if canPerformAction(&action, gameState) {
        actions.push(action);
        true
    } else {
        false
    }
}

fn canPerformAction(action: &Action, state: &GameState) -> bool
{
    match action {
        Action::Brew{id: _, ingredientsDelta: recipeIngredientsDelta, price: _} => canBrewRecipe(recipeIngredientsDelta, &state.ingredients),
        Action::Cast{id, ingredientsDelta: spellIngredientsDelta, times} => canCastSpell(*id, spellIngredientsDelta, *times, state),
        Action::Learn{id: _, ingredientsDelta: _, repeatable: _, readAheadTax} => canLearnSpell(*readAheadTax, &state.ingredients),
        Action::Rest => canRest(&state.actionRecipes),
        Action::Wait => true
    }
}

fn canBrewRecipe(recipeIngredientsDelta: &IngredientsDelta, ownedIngredients: &Ingredients) -> bool
{
    for (recipeIngredientDelta, ownedIngredient) in recipeIngredientsDelta.into_iter().zip(ownedIngredients) {
        if recipeIngredientDelta.abs() > *ownedIngredient {
            return false;
        }
    }
    true
}

fn canCastSpell(id: ActionId, spellIngredientsDelta: &IngredientsDelta, times: u32, gameState: &GameState) -> bool
{
    if isSpellExhausted(id, gameState) {
        return false;
    }

    let mut ownedIngredients = gameState.ingredients.clone();
    for _ in 0..times {
        applyDelta(&mut ownedIngredients, spellIngredientsDelta);

        let mut ingredientsCount = 0;
        for ingredient in &ownedIngredients {
            if *ingredient < 0 {
                return false;
            }
            ingredientsCount += ingredient;
        }
        if ingredientsCount > MAX_INGREDIENT_COUNT {
            return false;
        }
    }
    true
}

fn canLearnSpell(readAheadTax: ReadAheadTax, ownedIngredients: &Ingredients) -> bool
{
    readAheadTax <= (ownedIngredients[0] as ReadAheadTax)
}

fn canRest(actionRecipes: &[ActionRecipe]) -> bool
{
    for recipe in actionRecipes {
        match recipe {
            ActionRecipe::Cast {id: _, ingredientsDelta: _, castable, repeatable: _} => {
                if !castable {
                    return true;
                }
            },
            _ => ()
        }
    }
    false
}

fn isSpellExhausted(spellId: ActionId, gameState: &GameState) -> bool
{
    gameState.actionRecipes.iter().find(|recipe|
        if let ActionRecipe::Cast{id, ingredientsDelta, castable, repeatable} = recipe {
            *id == spellId && !castable
        } else {
            false
        }
    ).is_some()
}

const MAX_INGREDIENT_COUNT: i32 = 10;

fn calculateGameStateAfterAction(currentState: &GameState, action: &Action) -> GameState
{
    let mut newState = currentState.clone();
    match action {
        Action::Brew{id: _, ingredientsDelta, price} => {
            applyDelta(&mut newState.ingredients, &ingredientsDelta);
            newState.score += price;
            newState.potionsBrewed += 1;
        },
        Action::Cast{id, ingredientsDelta, times} => {
            for _ in 0..*times {
                applyDelta(&mut newState.ingredients, &ingredientsDelta);
            }
            match findSpellRecipe(*id, &mut newState.actionRecipes).unwrap() {
                ActionRecipe::Cast{id, ingredientsDelta, castable, repeatable} => *castable = false,
                _ => panic!("spell with id {} was not found in action recipes", id)
            }

        },
        Action::Learn{id, ingredientsDelta, repeatable, readAheadTax: _} => {
            let tomeSpellIndex = newState.actionRecipes.iter().enumerate().find(|(index, recipe)|
                if let ActionRecipe::Learn{id: currentId, ..} = recipe {
                    currentId == id
                } else {
                    false
                }
            ).unwrap().0;
            newState.actionRecipes.remove(tomeSpellIndex);
            // TODO new id is probably incorrect, instead make it an option and if not present, search by ingredients
            newState.actionRecipes.push(
                ActionRecipe::Cast{id: *id, ingredientsDelta: *ingredientsDelta, castable: true, repeatable: *repeatable});
            // TODO update readAheadTax of remaining spells to learn according to their new position (some will be unchanged, some will decrement)
            // TODO include taking taxCount here and in canPerformAction (check what are the rules if there are not enough slots in the inventory)
            // TODO include giving out ingredients according to readAheadTax
            // TODO do tome spells appear in random order?
        }
        Action::Rest => {
            refreshExhaustedSpells(&mut newState.actionRecipes)
        },
        Action::Wait => ()
    };
    newState.turns += 1;
    newState
}

fn applyDelta(ownedIngredients: &mut Ingredients, ingredientsDelta: &IngredientsDelta)
{
    for (owned, delta) in ownedIngredients.into_iter().zip(ingredientsDelta) {
        *owned += delta;
    }
}

fn findSpellRecipe(spellId: ActionId, actionRecipes: &mut Vec<ActionRecipe>) -> Option<&mut ActionRecipe>
{
    actionRecipes.iter_mut().find(|recipe|
        match recipe {
            ActionRecipe::Cast{id, ..} => *id == spellId,
            _ => false
        }
    )
}

fn refreshExhaustedSpells(actionRecipes: &mut Vec<ActionRecipe>)
{
    for recipe in actionRecipes {
        if let ActionRecipe::Cast{id, ingredientsDelta, castable, repeatable} = recipe {
            if !*castable {
                *castable = true;
            }
        }
    }
}

#[cfg(feature = "visualize")]
fn visualizeTree(tree: &Tree)
{
    let mut visualTree = id_tree::TreeBuilder::new().build();
    let rootNode = tree.getRoot().borrow().deref().clone();
    let visualRootId = visualTree.insert(id_tree::Node::new(rootNode.clone()), id_tree::InsertBehavior::AsRoot).unwrap();
    addChildrenToIdTree(&rootNode, &tree, visualRootId, &mut visualTree);
    id_tree_layout::Layouter::new(&visualTree).with_file_path(std::path::Path::new("tree.svg")).write().unwrap();
}

#[cfg(feature = "visualize")]
fn addChildrenToIdTree(node: &Node, tree: &Tree, visualNodeId: id_tree::NodeId, visualTree: &mut id_tree::Tree<Node>)
{
    for childNodeId in &node.children {
        let childNode = tree.getNode(*childNodeId).borrow().deref().clone();
        let visualChildId = visualTree.insert(id_tree::Node::new(childNode.clone()), id_tree::InsertBehavior::UnderNode(&visualNodeId)).unwrap();
        addChildrenToIdTree(&childNode, tree, visualChildId, visualTree);
    }
}

#[cfg(feature = "visualize")]
impl id_tree_layout::Visualize for Node
{
    fn visualize(&self) -> std::string::String
    {
        format!("id {}, action: {}, value: {}", self.id, formatActionShortly(&self.action), self.value)
    }
}

#[cfg(feature = "visualize")]
fn formatActionShortly(action: &Action) -> String
{
    match action {
        Action::Brew{id, ..} => format!("BREW {}", id),
        Action::Cast{id, ..} => format!("CAST {}", id),
        Action::Learn{id, ..} => format!("LEARN {}", id),
        Action::Rest => format!("REST"),
        Action::Wait => format!("WAIT")
    }
}
