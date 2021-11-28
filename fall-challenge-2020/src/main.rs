#![allow(non_snake_case)]

use std::cell::RefCell;
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
        let mut actionRecipes = vec![];
        gameState.exhaustedSpells.clear();
        for _ in 0..actionCount as usize {
            let mut inputLine = String::new();
            stdin().read_line(&mut inputLine).unwrap();
            // eprintln!("{}", inputLine);
            let inputs = inputLine.split(" ").collect::<Vec<_>>();
            let actionId = parseInput!(inputs[0], u32);
            let actionType = inputs[1].trim().to_string();
            if actionType == "OPPONENT_CAST" || actionType == "LEARN" {
                continue;
            }

            let delta0 = parseInput!(inputs[2], i32);
            let delta1 = parseInput!(inputs[3], i32);
            let delta2 = parseInput!(inputs[4], i32);
            let delta3 = parseInput!(inputs[5], i32);
            let price = parseInput!(inputs[6], i32);
            let _tomeIndex = parseInput!(inputs[7], i32);
            let _taxCount = parseInput!(inputs[8], i32);
            let castable = toBool(parseInput!(inputs[9], i32));
            let repeatable = toBool(parseInput!(inputs[10], i32));

            if actionType == "BREW" || (actionType == "CAST" && castable) {
                actionRecipes.push(ActionRecipe::new(
                    actionId,
                    &actionType,
                    [delta0, delta1, delta2, delta3],
                    price,
                    repeatable
                ));
            } else {
                gameState.exhaustedSpells.push(ExhaustedSpell::new(actionId));
            }
        }
        actionRecipes.push(ActionRecipe::Rest);
        // actions.push(Action::Wait);

        // eprintln!("Actions: {}", actions.len());
        // for (index, action) in actions.iter().enumerate() {
        // eprintln!("{}) action: {:?}", index, action);
        // }

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

        if actionPlan.is_empty() || !canPerformAction(&actionPlan[0], &gameState) {
            actionPlan = runMonteCarloTreeSearch(&gameState, &actionRecipes);
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
        Action::Brew {id, ..} => {
            println!("BREW {}", id);
            gameState.potionsBrewed += 1;
        },
        Action::Cast {id, ingredientsDelta: _, times} => println!("CAST {} {}", id, times),
        Action::Rest => println!("REST"),
        Action::Wait => println!("WAIT")
    };
    actionPlan.remove(0);
    gameState.turns += 1;

}

type ActionPlan = Vec<Action>;

#[derive(Debug)]
enum ActionRecipe
{
    Brew{id: ActionId, ingredientsDelta: IngredientsDelta, price: Price},
    Cast{id: ActionId, ingredientsDelta: IngredientsDelta, repeatable: bool},
    Rest
}

impl ActionRecipe
{
    fn new(id: ActionId, kind: &str, ingredientsDelta: IngredientsDelta, price: Price, repeatable: bool) -> Self
    {
        match kind {
            "BREW" => Self::Brew{id, ingredientsDelta, price},
            "CAST" => Self::Cast{id, ingredientsDelta, repeatable},
            _ => panic!("unexpected action recipe kind: {}", kind)
        }
    }
}

#[derive(Clone, Debug)]
enum Action
{
    Brew{id: ActionId, ingredientsDelta: IngredientsDelta, price: Price},
    Cast{id: ActionId, ingredientsDelta: IngredientsDelta, times: u32},
    Rest,
    Wait
}

type ActionId = u32;
type Price = i32;
type Score = i32;

fn runMonteCarloTreeSearch(gameState: &GameState, actionRecipes: &[ActionRecipe]) -> Vec<Action>
{
    let mut tree = Tree::new();
    let rootNode = tree.getRoot();
    fillRootNode(&rootNode, &gameState);
    makeChildNodesFromAvailableActions(&rootNode, &mut tree, actionRecipes);
    if rootNode.borrow().children.is_empty() {
        return vec![Action::Wait];
    }

    let mut currentNode = Rc::clone(&rootNode);
    for _ in 0..1000 {
        if currentNode.borrow().isLeaf() {
            if currentNode.borrow().visits == 0 {
                let score = rollout(&currentNode, actionRecipes);
                backpropagate(&currentNode, score, &tree);
                currentNode = findBestChildNodeToExplore(&rootNode, &tree).unwrap();
            } else {
                makeChildNodesFromAvailableActions(&currentNode, &mut tree, actionRecipes);
                if currentNode.borrow().children.is_empty() {
                    break;
                }
                let childNodeId = currentNode.borrow().children[0];
                currentNode = tree.getNode(childNodeId);
                let score = rollout(&currentNode, actionRecipes);
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

fn rollout(currentNode: &Rc<RefCell<Node>>, actionRecipes: &[ActionRecipe]) -> Score
{
    let mut state = currentNode.borrow().state.clone();
    loop {
        let possibleActions = makePerformableActionsFromRecipes(actionRecipes, &state);
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
const MAX_POTIONS: u32 = 3;

fn chooseRandomAction(actions: &[Action]) -> &Action
{
    &actions[rand::random::<usize>() % actions.len()]
}

fn calculateStateScore(state: &GameState) -> Score
{
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
    exhaustedSpells: Vec<ExhaustedSpell>,
    score: i32,
    potionsBrewed: u32,
    turns: u32
}

impl GameState
{
    fn new() -> Self
    {
        Self{ingredients: [0, 0, 0, 0], exhaustedSpells: vec![], score: 0, potionsBrewed: 0, turns: 0}
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

fn makeChildNodesFromAvailableActions(currentNode: &Rc<RefCell<Node>>, tree: &mut Tree, actionRecipes: &[ActionRecipe])
{
    let mut currentNode = currentNode.borrow_mut();
    for actionRecipe in actionRecipes {
        let performableActions = makePerformableActionsFromRecipe(actionRecipe, &currentNode.state);
        for action in performableActions {
            let childNodeId = tree.addNode(calculateGameStateAfterAction(&currentNode.state, &action), currentNode.id, action);
            currentNode.children.push(childNodeId);
        }
    }
}

fn makePerformableActionsFromRecipes(actionRecipes: &[ActionRecipe], gameState: &GameState) -> Vec<Action>
{
    let mut actions = vec![];
    for actionRecipe in actionRecipes {
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
        ActionRecipe::Cast{id, ingredientsDelta, repeatable} => {
            actions = makePerformableCastsFromRecipe(*id, ingredientsDelta, *repeatable, gameState)
        },
        ActionRecipe::Rest => {
            pushIfCanPerformAction(Action::Rest, &mut actions, gameState);
        }
    }
    actions
}

fn makePerformableCastsFromRecipe(id: ActionId, ingredientsDelta: &IngredientsDelta, repeatable: bool, gameState: &GameState) -> Vec<Action>
{
    let mut actions = vec![];
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
        Action::Rest => canRest(&state.exhaustedSpells),
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
    match gameState.exhaustedSpells.iter().find(|spell| spell.id == id) {
        Some(_) => return false,
        None => ()
    };

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

fn canRest(exhaustedSpells: &[ExhaustedSpell]) -> bool
{
    !exhaustedSpells.is_empty()
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
            newState.exhaustedSpells.push(ExhaustedSpell::new(*id));
        },
        Action::Rest => {
            newState.exhaustedSpells.clear();
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
        Action::Rest => format!("REST"),
        Action::Wait => format!("WAIT")
    }
}
