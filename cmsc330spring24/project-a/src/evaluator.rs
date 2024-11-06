//! This file is where you will implement your code.

use std::{
    collections::{BTreeSet, HashMap},
    sync::Arc,
};

use crate::{Fsm, VendingAction, DENOMINATIONS, MAX_CENTS};


/*
/// The list of coin denominations accepted by the machine, in cents.
const DENOMINATIONS: [u32; 3] = [5, 10, 25];

/// The maximum amount of money that the machine can hold (in cents).
const MAX_CENTS: u32 = 500;

pub struct Fsm {
    /// The ID of the initial state.
    /// (ID values can be whatever you want,
    /// as long as each ID uniquely identifies a state.)
    pub start: u32,

    /// A map representing possible transitions:
    /// (initial state, transition symbol) → final state.
    pub transitions: HashMap<(u32, VendingAction), u32>,
}
*/

impl Fsm {
    pub fn create(price_map: &HashMap<Arc<str>, u32>) -> Fsm {
      let mut fsm = Fsm {
        start: 0,
        transitions: HashMap::new(),
    };

    for cent in 0..= MAX_CENTS {
        for denomination in DENOMINATIONS {
            if cent + denomination <= MAX_CENTS {
                fsm.transitions.insert(
                    (cent, VendingAction::Insert(denomination)),
                    cent + denomination
                );
            }
            //println!("cent: {}, denomination: {}", cent, denomination);
        }

        for (item, price) in price_map {
            if cent >= *price {
              let copy = item.clone();
                fsm.transitions.insert(
                    (cent, VendingAction::Vend(copy)),
                    cent - price
                );
            }
            //println!("cent: {}, item: {}, price: {}", cent, item, price);
        }
    }
    return fsm
}

    pub fn eval(&self, actions: &[VendingAction]) -> bool {
      let mut curr = 0; 
      for action in actions {
        let copy = action.clone();
        let key = (curr, copy);
        match self.transitions.get(&key) {
          Some(&next) => curr = next,
          None => return false,
        }
      }
      return true
    }
}
