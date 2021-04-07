use super::rule_table::*;
use std::cmp::Ordering;
use std::collections::HashMap;

#[derive(Debug)]
pub struct NormalizedRuleTable {
    pub rules: Vec<NormalizedRule>,
    pub rule_name_index_map: HashMap<String, Vec<usize>>,
}

#[derive(Clone, Debug)]
pub struct NormalizedRule {
    pub index: usize,
    pub name: String,
    pub reduce_name: String,
    pub origin_reduce_name: String,
    pub origin_type: Vec<NormalizedRuleOriginType>,
    pub items: Vec<NormalizedRuleItem>,
    pub is_internal: bool,
}

impl NormalizedRule {
    pub fn first_item(&self) -> Option<&NormalizedRuleItem> {
        self.items.first()
    }
}

impl PartialEq for NormalizedRule {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.items == other.items
    }
}

impl Eq for NormalizedRule {}

impl PartialOrd for NormalizedRule {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NormalizedRule {
    fn cmp(&self, other: &Self) -> Ordering {
        (&self.name, &self.items).cmp(&(&other.name, &other.items))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum NormalizedRuleItem {
    Terminal(String),
    NonTerminal(String),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum NormalizedRuleOriginType {
    Terminal,
    NonTerminal(String),
    OptionSome(Vec<NormalizedRuleOriginType>),
    OptionNone,
    Repeat(String),
    RepeatEnd(String),
    RepeatContinue(String),
}

impl NormalizedRuleItem {
    pub fn is_terminal(&self) -> bool {
        if let NormalizedRuleItem::Terminal(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_non_terminal(&self) -> bool {
        if let NormalizedRuleItem::NonTerminal(..) = self {
            true
        } else {
            false
        }
    }

    pub fn inner(&self) -> &String {
        match self {
            NormalizedRuleItem::Terminal(terminal) => terminal,
            NormalizedRuleItem::NonTerminal(non_terminal) => non_terminal,
        }
    }

    pub fn terminal(&self) -> &String {
        if let NormalizedRuleItem::Terminal(terminal) = self {
            terminal
        } else {
            unreachable!()
        }
    }

    pub fn non_terminal(&self) -> &String {
        if let NormalizedRuleItem::NonTerminal(non_terminal) = self {
            non_terminal
        } else {
            unreachable!()
        }
    }
}

fn normalize_options(rule: Rule) -> Vec<(Vec<RuleItem>, Vec<usize>, Vec<(usize, usize)>)> {
    let mut items = vec![(rule.items, Vec::new(), Vec::new())];
    let mut index = 0;

    while index < items.len() {
        let mut extra_items = Vec::new();

        {
            let items = &mut items[index];
            let mut index = 0;

            while index < items.0.len() {
                if let RuleItem::Option(option_items) = &items.0[index] {
                    let mut extra_item = items.clone();
                    let mut rest_items = extra_item.0.split_off(index + 1);
                    extra_item.0.pop();
                    extra_item.0.extend_from_slice(option_items);
                    extra_item.0.append(&mut rest_items);
                    extra_item.2.push((index, option_items.len()));
                    extra_items.push(extra_item);

                    items.0.remove(index);
                    items.1.push(index);
                } else {
                    index += 1;
                }
            }
        }

        items.extend(extra_items);
        index += 1;
    }

    items
}

fn normalize_repeat(rule: Rule) -> Vec<NormalizedRule> {
    let name = rule.name.clone();
    let reduce_name = rule.reduce_name.clone();
    let mut rules = Vec::new();
    let mut items = normalize_options(rule).into_iter().enumerate().collect::<Vec<_>>();

    for (index, items) in items.iter_mut() {
        for (inner_index, item) in items.0.iter_mut().enumerate() {
            if let RuleItem::Repeat(repeat_items) = item {
                let internal_rule_name = format!("_{}-{}", name, index);
                let mut internal_rule_items = Vec::new();

                internal_rule_items.append(repeat_items);
                internal_rule_items.push(RuleItem::Option(vec![RuleItem::NonTerminal(internal_rule_name.clone())]));

                *item = RuleItem::NonTerminal(internal_rule_name.clone());

                rules.extend(
                    normalize_repeat(Rule {
                        name: internal_rule_name.clone(),
                        reduce_name: format!("_{}-{}-{}", name, index, reduce_name),
                        items: internal_rule_items,
                    })
                    .into_iter()
                    .map(|mut rule| {
                        rule.is_internal = true;
                        rule
                    }),
                );

                // items.1 .0[inner_index] = NormalizedRuleOriginType::Repeat(internal_rule_name);
            }
        }
    }

    for (index, items) in items {
        if items.0.is_empty() {
            continue;
        }

        let mut ty = items
            .0
            .iter()
            .map(|rule_item| match rule_item {
                RuleItem::Terminal(..) => NormalizedRuleOriginType::Terminal,
                RuleItem::NonTerminal(non_terminal) => NormalizedRuleOriginType::NonTerminal(non_terminal.clone()),
                _ => unreachable!(),
            })
            .collect::<Vec<_>>();

        for index in items.1 {
            ty.insert(index, NormalizedRuleOriginType::OptionNone);
        }

        for range in items.2.into_iter().rev() {
            let mut rest = ty.split_off(range.0);
            let other = rest.split_off(range.1);
            ty.push(NormalizedRuleOriginType::OptionSome(rest));
            ty.extend(other);
        }

        rules.push(NormalizedRule {
            index: 0,
            name: name.clone(),
            reduce_name: format!("{}-reduce-{}", reduce_name, index),
            origin_reduce_name: reduce_name.clone(),
            origin_type: ty,
            items: items
                .0
                .into_iter()
                .map(|item| match item {
                    RuleItem::Terminal(terminal) => NormalizedRuleItem::Terminal(terminal),
                    RuleItem::NonTerminal(non_terminal) => NormalizedRuleItem::NonTerminal(non_terminal),
                    _ => unreachable!(),
                })
                .collect(),
            is_internal: false,
        })
    }

    rules
}

pub fn normalize_rule_table(rule_table: RuleTable) -> NormalizedRuleTable {
    let mut rules = Vec::new();

    for rule in rule_table {
        rules.extend(normalize_repeat(rule));
    }

    rules.sort();
    rules.dedup();

    // Filter-out rules that have self-reference only.
    rules = rules
        .into_iter()
        .filter(|rule| {
            if rule.items.len() == 1 {
                if let NormalizedRuleItem::NonTerminal(non_terminal) = &rule.items[0] {
                    return &rule.name != non_terminal;
                }
            }

            true
        })
        .collect();

    let mut rule_name_index_map = HashMap::<_, Vec<_>>::new();
    let mut index = 0;

    for rule in rules.iter_mut() {
        rule.index = index;
        rule_name_index_map.entry(rule.name.clone()).or_default().push(index);

        index += 1;
    }

    NormalizedRuleTable { rules, rule_name_index_map }
}
