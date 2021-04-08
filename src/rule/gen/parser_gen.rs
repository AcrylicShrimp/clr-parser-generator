use super::super::normalized_rule_table::*;
use itertools::Itertools;
use std::cmp::{Eq, PartialEq};
use std::collections::hash_map::Entry;
use std::collections::BTreeSet;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::hash::Hash;
use std::sync::mpsc::channel;
use std::sync::Arc;
use std::vec::Vec;

pub type FirstSet = HashMap<String, HashSet<String>>;

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ARule {
    pub rule_index: usize,
    pub seen_index: usize,
    pub lookahead: String,
}

impl ARule {
    pub fn new(rule_index: usize, lookahead: String) -> ARule {
        ARule {
            rule_index: rule_index,
            seen_index: 0,
            lookahead: lookahead,
        }
    }
}

pub type State = BTreeSet<ARule>;
pub type StateHandle = Arc<State>;
pub type StateTransitionSource = (usize, String);

#[derive(Debug)]
pub struct StateGraph {
    pub state_vec: Vec<StateHandle>,
    pub state_map: HashMap<StateHandle, usize>,
    pub state_transition_map: HashMap<StateTransitionSource, usize>,
}

impl StateGraph {
    pub fn new() -> StateGraph {
        StateGraph {
            state_vec: Vec::new(),
            state_map: HashMap::new(),
            state_transition_map: HashMap::new(),
        }
    }

    pub fn add_state(&mut self, state_handle: StateHandle) -> usize {
        match self.state_map.entry(state_handle.clone()) {
            Entry::Occupied(index) => *index.get(),
            Entry::Vacant(index) => {
                let state_index = self.state_vec.len();

                index.insert(state_index);
                self.state_vec.push(state_handle);

                state_index
            }
        }
    }
}

#[derive(PartialEq)]
pub enum ActionType {
    Shift,
    Reduce,
    Goto,
    Accept,
}

#[derive(PartialEq)]
pub struct Action {
    pub action_type: ActionType,
    pub next_state: usize,
    pub reduce_count: usize,
    pub reduce_rule_name: String,
    pub rule_name: String,
}

pub struct ActionMap {
    pub id: HashMap<String, Action>,
    pub literal: HashMap<String, Action>,
}

impl ActionMap {
    pub fn new() -> ActionMap {
        ActionMap {
            id: HashMap::new(),
            literal: HashMap::new(),
        }
    }
}

pub type ActionTable = Vec<ActionMap>;

pub struct Gen {
    pub rule_table: NormalizedRuleTable,
    pub first_set: FirstSet,
    pub state_graph: StateGraph,
    pub action_table: ActionTable,
}

impl Gen {
    pub fn new(rule_table: NormalizedRuleTable) -> Gen {
        Gen {
            rule_table: rule_table,
            first_set: FirstSet::new(),
            state_graph: StateGraph::new(),
            action_table: ActionTable::new(),
        }
    }
}

impl Gen {
    pub fn generate_first(&mut self) {
        for (rule_name, _) in &self.rule_table.rule_name_index_map {
            self.first_set.insert(rule_name.clone(), HashSet::new());
        }

        loop {
            let mut updated = false;
            for rule in &self.rule_table.rules {
                match rule.first_item() {
                    Some(rule_item) => {
                        updated = updated || Gen::update_set(&mut self.first_set, &rule.name, rule_item);
                    }
                    _ => (),
                }
            }

            if !updated {
                break;
            }
        }
    }

    fn update_set(first_set: &mut FirstSet, rule_name: &String, rule_item: &NormalizedRuleItem) -> bool {
        let mut first_item_vec: Vec<String> = Vec::new();

        match rule_item {
            NormalizedRuleItem::Terminal(terminal) => first_item_vec.push(terminal.clone()),
            NormalizedRuleItem::NonTerminal(non_ternimal) => {
                for src_first_set_item in {
                    match first_set.get(non_ternimal) {
                        Some(first_set) => first_set,
                        None => panic!("{} not found in first set!!!", non_ternimal),
                    }
                }
                .iter()
                {
                    first_item_vec.push(src_first_set_item.clone());
                }
            }
        }

        let first = first_set.get_mut(rule_name).unwrap();
        let first_item_count = first.len();

        for first_item in first_item_vec {
            first.insert(first_item);
        }

        first_item_count != first.len()
    }
}

impl Gen {
    fn closure(&self, state: &mut State) {
        loop {
            let size = state.len();
            let mut rules = Vec::new();

            for rule in state.iter() {
                let src_rule = &self.rule_table.rules[rule.rule_index];

                if src_rule.items.len() <= rule.seen_index {
                    continue;
                }

                let marker_item = &src_rule.items[rule.seen_index];

                if let NormalizedRuleItem::Terminal(..) = marker_item {
                    continue;
                }

                let mut lookahead_set = HashSet::new();

                if rule.seen_index + 1 < src_rule.items.len() {
                    let lookahead_rule_item = &src_rule.items[rule.seen_index + 1];

                    match lookahead_rule_item {
                        NormalizedRuleItem::Terminal(terminal) => {
                            lookahead_set.insert(terminal.clone());
                        }
                        NormalizedRuleItem::NonTerminal(non_terminal) => {
                            let src_first_set = &self.first_set[non_terminal];
                            if src_first_set.is_empty() {
                                lookahead_set.insert("".to_string());
                            } else {
                                for first in src_first_set {
                                    lookahead_set.insert(first.clone());
                                }
                            }
                        }
                    }
                } else {
                    lookahead_set.insert("".to_string());
                }

                if lookahead_set.len() == 1 && lookahead_set.contains("") {
                    lookahead_set = HashSet::new();
                    lookahead_set.insert(rule.lookahead.clone());
                }

                for rule_index in &self.rule_table.rule_name_index_map[marker_item.inner()] {
                    for lookahead in lookahead_set.iter() {
                        rules.push(ARule::new(*rule_index, lookahead.clone()));
                    }
                }
            }

            for rule in rules {
                state.insert(rule);
            }

            if size == state.len() {
                break;
            }
        }
    }

    fn closure_rule(&self, rule: ARule) -> State {
        let mut state = State::new();
        state.insert(rule);

        self.closure(&mut state);

        state
    }

    fn goto(&self, state: &State, label: &String) -> Option<StateHandle> {
        let mut result_state = State::new();

        for rule in state.iter() {
            let src_rule = &self.rule_table.rules[rule.rule_index];

            if src_rule.items.len() <= rule.seen_index {
                continue;
            }

            if src_rule.items[rule.seen_index].inner() != label {
                continue;
            }

            result_state.insert(ARule {
                rule_index: rule.rule_index,
                seen_index: rule.seen_index + 1,
                lookahead: rule.lookahead.clone(),
            });
        }

        if result_state.is_empty() {
            None
        } else {
            self.closure(&mut result_state);
            Some(StateHandle::new(result_state))
        }
    }

    pub fn generate_dag(&mut self) {
        if !self.rule_table.rule_name_index_map.contains_key("__root") {
            panic!("no root nonterminal found");
        }

        if self.rule_table.rule_name_index_map["__root"].len() != 1 {
            panic!("multiple root nonterminal found");
        }

        let mut state_index_vec: Vec<usize> = vec![self
            .state_graph
            .add_state(StateHandle::new(self.closure_rule(ARule::new(self.rule_table.rule_name_index_map["__root"][0], "".to_string()))))];

        let mut thread_pool = scoped_threadpool::Pool::new(num_cpus::get().try_into().unwrap());
        let (sender, receiver) = channel::<Vec<(usize, String, StateHandle, Option<usize>)>>();

        while !state_index_vec.is_empty() {
            thread_pool.scoped(|scoped| {
                for index in 0..state_index_vec.len() {
                    let this = &*self;
                    let thread_sender = sender.clone();
                    let state_index = state_index_vec[index];

                    scoped.execute(move || {
                        match thread_sender.send(
                            this.state_graph.state_vec[state_index]
                                .iter()
                                .filter(|rule| rule.seen_index < this.rule_table.rules[rule.rule_index].items.len())
                                .map(|rule| {
                                    let state = this.goto(&this.state_graph.state_vec[state_index], this.rule_table.rules[rule.rule_index].items[rule.seen_index].inner());
                                    let previous_state_index = if state.is_some() {
                                        match this.state_graph.state_map.get(state.as_ref().unwrap()) {
                                            Some(state_index) => Some(*state_index),
                                            None => None,
                                        }
                                    } else {
                                        None
                                    };

                                    (state_index, this.rule_table.rules[rule.rule_index].items[rule.seen_index].inner().clone(), state, previous_state_index)
                                })
                                .filter(|transition| transition.2.is_some())
                                .map(|transition| (transition.0, transition.1, transition.2.unwrap(), transition.3))
                                .collect(),
                        ) {
                            Ok(_) => (),
                            Err(_) => panic!(),
                        };
                    });
                }
            });

            state_index_vec = receiver
                .iter()
                .take(state_index_vec.len())
                .flatten()
                .map(|transition| {
                    let state_index = match transition.3 {
                        Some(state_index) => state_index,
                        None => self.state_graph.add_state(transition.2),
                    };

                    self.state_graph.state_transition_map.insert((transition.0, transition.1), state_index);

                    match transition.3 {
                        Some(_) => None,
                        None => Some(state_index),
                    }
                })
                .flatten()
                .unique()
                .collect();
        }
    }
}

impl Gen {
    pub fn generate_action_table(&mut self) {
        for _ in 0..self.state_graph.state_vec.len() {
            self.action_table.push(ActionMap::new());
        }

        for state_index in 0..self.state_graph.state_vec.len() {
            for rule in self.state_graph.state_vec[state_index].iter() {
                let src_rule = &self.rule_table.rules[rule.rule_index];

                if rule.seen_index < src_rule.items.len() && src_rule.items[rule.seen_index].is_terminal() {
                    // Shift
                    let table = &mut self.action_table[state_index].literal;
                    let item = src_rule.items[rule.seen_index].terminal().clone();
                    let action = Action {
                        action_type: ActionType::Shift,
                        next_state: *self.state_graph.state_transition_map.get(&(state_index, src_rule.items[rule.seen_index].terminal().clone())).unwrap(),
                        reduce_count: 0,
                        reduce_rule_name: "".to_string(),
                        rule_name: "".to_string(),
                    };

                    if table.contains_key(&item) {
                        let old_action = table.get(&item).unwrap();

                        if action != *old_action {
                            panic!("ambiguous syntax; shift conflict. {}", src_rule.name);
                        }
                    } else {
                        table.insert(item, action);
                    }
                } else if rule.seen_index < src_rule.items.len() && src_rule.items[rule.seen_index].is_non_terminal() {
                    // Goto
                    let table = &mut self.action_table[state_index].id;
                    let item = src_rule.items[rule.seen_index].non_terminal().clone();
                    let action = Action {
                        action_type: ActionType::Goto,
                        next_state: *self
                            .state_graph
                            .state_transition_map
                            .get(&(state_index, src_rule.items[rule.seen_index].non_terminal().clone()))
                            .unwrap(),
                        reduce_count: 0,
                        reduce_rule_name: "".to_string(),
                        rule_name: "".to_string(),
                    };

                    if table.contains_key(&item) {
                        let old_action = table.get(&item).unwrap();

                        if action != *old_action {
                            panic!("ambiguous syntax; goto conflict. {}", src_rule.name);
                        }
                    } else {
                        table.insert(item, action);
                    }
                } else if rule.lookahead == "" && src_rule.name == "__root" {
                    //Accept
                    let table = &mut self.action_table[state_index].literal;
                    let item = "";
                    let action = Action {
                        action_type: ActionType::Accept,
                        next_state: 0,
                        reduce_count: 0,
                        reduce_rule_name: "".to_string(),
                        rule_name: "".to_string(),
                    };

                    if table.contains_key(item) {
                        let old_action = table.get(item).unwrap();

                        if action != *old_action {
                            panic!("ambiguous syntax; accept conflict. {}", src_rule.name);
                        }
                    } else {
                        table.insert(item.to_owned(), action);
                    }
                } else {
                    // Reduce
                    let table = &mut self.action_table[state_index].literal;
                    let item = rule.lookahead.clone();
                    let action = Action {
                        action_type: ActionType::Reduce,
                        next_state: 0,
                        reduce_count: src_rule.items.len(),
                        reduce_rule_name: format!("{}{}", src_rule.name, src_rule.reduce_name),
                        rule_name: src_rule.name.clone(),
                    };

                    if table.contains_key(&item) {
                        let old_action = table.get(&item).unwrap();

                        if action != *old_action {
                            panic!("ambiguous syntax; reduce conflict. {}", src_rule.name);
                        }
                    } else {
                        table.insert(item, action);
                    }
                }
            }
        }
    }
}
