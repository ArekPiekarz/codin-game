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
        gameState.recipes.clear();
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

            gameState.addRecipe(
                actionId,
                &actionType,
                [delta0, delta1, delta2, delta3],
                price,
                readAheadTaxOrUrgencyBonus,
                castable,
                repeatable);
        }

        gameState.printRecipes();

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
        Action::Cast{idOpt, ingredientsDelta, times} => performCast(idOpt, &ingredientsDelta, times, &gameState.recipes.casting),
        Action::Learn{id, ..} => println!("LEARN {}", id),
        Action::Rest => println!("REST"),
        Action::Wait => println!("WAIT")
    };
    actionPlan.remove(0);
    gameState.turns += 1;

}

type ActionPlan = Vec<Action>;

fn performCast(idOpt: Option<Id>, ingredientsDelta: &IngredientsDelta, times: CastTimes, recipes: &[CastRecipe])
{
    let id = match idOpt {
        Some(id) => id,
        None => findSpellIdByIngredients(ingredientsDelta, recipes)
    };
    // TODO optimization - fix id in the rest of actionPlan
    println!("CAST {} {}", id, times);
}

fn findSpellIdByIngredients(ingredientsDelta: &IngredientsDelta, recipes: &[CastRecipe]) -> Id
{
    assertOnlyOneSpell(ingredientsDelta, recipes);
    recipes.iter().find(|recipe| recipe.ingredientsDelta == *ingredientsDelta).unwrap().idOpt.unwrap()
}

#[derive(Clone, Debug)]
struct Recipes
{
    brewing: Vec<BrewRecipe>,
    learning: Vec<LearnRecipe>,
    casting: Vec<CastRecipe>
}

impl Recipes
{
    fn new() -> Self
    {
        Self{brewing: vec![], learning: vec![], casting: vec![]}
    }

    fn clear(&mut self)
    {
        self.brewing.clear();
        self.learning.clear();
        self.casting.clear();
    }
}

#[derive(Clone, Debug)]
struct BrewRecipe
{
    id: Id,
    ingredientsDelta: IngredientsDelta,
    price: Price
}

#[derive(Clone, Debug)]
struct LearnRecipe
{
    id: Id,
    ingredientsDelta: IngredientsDelta,
    repeatable: bool,
    readAheadTax: u32
}

#[derive(Clone, Debug)]
struct CastRecipe
{
    idOpt: Option<Id>,
    ingredientsDelta: IngredientsDelta,
    castable: bool,
    repeatable: bool
}

type Id = u32;
type ReadAheadTax = u32;

#[derive(Clone, Debug)]
enum Action
{
    Brew{id: Id, ingredientsDelta: IngredientsDelta, price: Price},
    Learn{id: Id, ingredientsDelta: IngredientsDelta, repeatable: bool, readAheadTax: ReadAheadTax},
    Cast{idOpt: Option<Id>, ingredientsDelta: IngredientsDelta, times: CastTimes},
    Rest,
    Wait
}

type Price = i32;
type Score = i32;
type CastTimes = u32;

fn runMonteCarloTreeSearch(gameState: &GameState) -> Vec<Action>
{
    let mut tree = Tree::new();
    let rootNode = tree.getRoot();
    fillRootNode(&rootNode, &gameState);
    makeChildNodesFromRecipes(&rootNode, &mut tree);
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
                makeChildNodesFromRecipes(&currentNode, &mut tree);
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
    rootNode.borrow_mut().state = gameState.clone();
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
    recipes: Recipes,
    score: i32,
    potionsBrewed: u32,
    turns: u32
}

impl GameState
{
    fn new() -> Self
    {
        Self{ingredients: [0, 0, 0, 0], recipes: Recipes::new(), score: 0, potionsBrewed: 0, turns: 0}
    }

    fn addRecipe(
        &mut self,
        id: Id,
        kind: &str,
        ingredientsDelta: IngredientsDelta,
        price: Price,
        readAheadTaxOrUrgencyBonus: i32,
        castable: bool,
        repeatable: bool)
    {
        match kind {
            "BREW"  => self.recipes.brewing.push(BrewRecipe{id, ingredientsDelta, price}),
            "CAST"  => self.recipes.casting.push(CastRecipe{idOpt: Some(id), ingredientsDelta, castable, repeatable}),
            "LEARN" => self.recipes.learning.push(LearnRecipe{
                id, ingredientsDelta, repeatable, readAheadTax: readAheadTaxOrUrgencyBonus as ReadAheadTax}),
            _ => panic!("unexpected action recipe kind: {}", kind)
        }
    }

    fn printRecipes(&self)
    {
        eprintln!("Recipes: {}", self.countRecipes());
        let mut index = 0;
        for recipe in &self.recipes.brewing {
            eprintln!("{}) {:?}", index, recipe);
            index += 1;
        }
        for recipe in &self.recipes.learning {
            eprintln!("{}) {:?}", index, recipe);
            index += 1;
        }
        for recipe in &self.recipes.casting {
            eprintln!("{}) {:?}", index, recipe);
            index += 1;
        }
    }

    fn countRecipes(&self) -> usize
    {
        self.recipes.brewing.len() + self.recipes.learning.len() + self.recipes.casting.len()
    }
}

fn makeChildNodesFromRecipes(currentNode: &Rc<RefCell<Node>>, tree: &mut Tree)
{
    let mut currentNode = currentNode.borrow_mut();
    let currentNode = currentNode.deref_mut();
    let performableActions = makePerformableActionsFromRecipes(&currentNode.state);
    makeChildNodesFromPerformableActions(performableActions, currentNode, tree);
}

fn makeChildNodesFromPerformableActions(actions: Vec<Action>, currentNode: &mut Node, tree: &mut Tree)
{
    for action in actions {
        let childNodeId = tree.addNode(
            calculateGameStateAfterAction(&currentNode.state, &action), currentNode.id, action);
        currentNode.children.push(childNodeId);
    }
}

fn makePerformableActionsFromRecipes(gameState: &GameState) -> Vec<Action>
{
    let mut actions = vec![];
    for recipe in &gameState.recipes.brewing {
        actions.extend(makePerformableActionsFromBrewRecipe(recipe, &gameState));
    }
    for recipe in &gameState.recipes.learning {
        actions.extend(makePerformableActionsFromLearnRecipe(recipe, &gameState));
    }
    for recipe in &gameState.recipes.casting {
        actions.extend(makePerformableActionsFromCastRecipe(recipe, &gameState));
    }
    actions.extend(makePerformableActionsForResting(&gameState.recipes.casting));
    actions
}

fn makePerformableActionsFromBrewRecipe(recipe: &BrewRecipe, gameState: &GameState) -> Vec<Action>
{
    if canBrewRecipe(&recipe.ingredientsDelta, &gameState.ingredients) {
        vec![Action::Brew{id: recipe.id, ingredientsDelta: recipe.ingredientsDelta, price: recipe.price}]
    } else {
        vec![]
    }
}

fn makePerformableActionsFromCastRecipe(recipe: &CastRecipe, gameState: &GameState) -> Vec<Action>
{
    let mut actions = vec![];
    if !recipe.castable {
        return actions;
    }

    if recipe.repeatable {
        loop {
            let action = Action::Cast{
                idOpt: recipe.idOpt, ingredientsDelta: recipe.ingredientsDelta, times: (actions.len() as u32) + 1};
            if !pushIfCanPerformAction(action, &mut actions, gameState) {
                break;
            }
        }
    } else {
        pushIfCanPerformAction(
            Action::Cast{idOpt: recipe.idOpt, ingredientsDelta: recipe.ingredientsDelta, times: 1}, &mut actions, gameState);
    }
    actions
}

fn makePerformableActionsFromLearnRecipe(recipe: &LearnRecipe, gameState: &GameState) -> Vec<Action>
{
    if canLearnSpell(recipe.id, recipe.readAheadTax, gameState) {
        vec![Action::Learn{
            id: recipe.id,
            ingredientsDelta: recipe.ingredientsDelta,
            repeatable: recipe.repeatable,
            readAheadTax: recipe.readAheadTax}]
    } else {
        vec![]
    }
}

fn makePerformableActionsForResting(recipes: &[CastRecipe]) -> Vec<Action>
{
    if canRest(recipes) {
        vec![Action::Rest]
    } else {
        vec![]
    }
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
        Action::Cast{idOpt: _, ingredientsDelta: spellIngredientsDelta, times} => canCastSpell(spellIngredientsDelta, *times, state),
        Action::Learn{id, ingredientsDelta: _, repeatable: _, readAheadTax} => canLearnSpell(*id, *readAheadTax, state),
        Action::Rest => canRest(&state.recipes.casting),
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

fn canCastSpell(spellIngredientsDelta: &IngredientsDelta, times: u32, gameState: &GameState) -> bool
{
    // TODO check if id is on the list of recipes?
    if isSpellExhausted(spellIngredientsDelta, gameState) {
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

fn canLearnSpell(id: Id, readAheadTax: ReadAheadTax, gameState: &GameState) -> bool
{
    gameState.recipes.learning.iter().find(|recipe| recipe.id == id).is_some()
    && readAheadTax <= (gameState.ingredients[0] as ReadAheadTax)
}

fn canRest(recipes: &[CastRecipe]) -> bool
{
    for recipe in recipes {
        if !recipe.castable {
            return true;
        }
    }
    false
}

fn isSpellExhausted(ingredientsDelta: &IngredientsDelta, gameState: &GameState) -> bool
{
    assertOnlyOneSpell(ingredientsDelta, &gameState.recipes.casting);
    gameState.recipes.casting.iter().find(|recipe|
        recipe.ingredientsDelta == *ingredientsDelta && !recipe.castable).is_some()
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
        Action::Cast{idOpt: _, ingredientsDelta, times} => {
            for _ in 0..*times {
                applyDelta(&mut newState.ingredients, &ingredientsDelta);
            }
            exhaustSpell(ingredientsDelta, &mut newState);
        },
        Action::Learn{id: _, ingredientsDelta, repeatable, readAheadTax} => {
            let spellIndex = *readAheadTax as usize;
            newState.recipes.learning.remove(spellIndex);
            for recipe in &mut newState.recipes.learning[spellIndex..] {
                recipe.readAheadTax -= 1;
            }
            newState.recipes.casting.push(
                CastRecipe{idOpt: None, ingredientsDelta: *ingredientsDelta, castable: true, repeatable: *repeatable});
            // TODO include taking taxCount here and in canPerformAction (check what are the rules if there are not enough slots in the inventory)
            // TODO include giving out ingredients according to readAheadTax - need support for taxCount field
            // TODO do tome spells appear in random order?
        }
        Action::Rest => {
            refreshExhaustedSpells(&mut newState.recipes.casting)
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

fn exhaustSpell(ingredientsDelta: &IngredientsDelta, gameState: &mut  GameState)
{
    assertOnlyOneSpell(ingredientsDelta, &gameState.recipes.casting);
    gameState.recipes.casting.iter_mut().find(|recipe| recipe.ingredientsDelta == *ingredientsDelta)
        .unwrap().castable = false;
}

fn refreshExhaustedSpells(recipes: &mut Vec<CastRecipe>)
{
    for recipe in recipes {
        if !recipe.castable {
            recipe.castable = true;
        }
    }
}

fn assertOnlyOneSpell(ingredientsDelta: &IngredientsDelta, recipes: &[CastRecipe])
{
    assert_eq!(recipes.iter().filter(|recipe| recipe.ingredientsDelta == *ingredientsDelta).count(), 1);
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
        Action::Cast{idOpt, ingredientsDelta, times} => formatCastShortly(*idOpt, ingredientsDelta, *times),
        Action::Learn{id, ..} => format!("LEARN {}", id),
        Action::Rest => format!("REST"),
        Action::Wait => format!("WAIT")
    }
}

#[cfg(feature = "visualize")]
fn formatCastShortly(idOpt: Option<Id>, ingredientsDelta: &IngredientsDelta, times: CastTimes) -> String
{
    match idOpt {
        Some(id) => format!("CAST {} {}", id, times),
        None => format!("CAST ? {} ({:?})", times, ingredientsDelta)
    }
}
