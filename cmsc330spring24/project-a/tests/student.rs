/*
 * Create a new function for each test that you want to run.  Please be sure to add
 * the #[test] attribute to each of your student tests to ensure they are all run, and
 * prefix them all with 'student_' (see example below).
 * Then, run `cargo test student` to run all of the student tests.
 */
 use std::{collections::HashMap, sync::Arc};
 use vending_machine::{Fsm, VendingAction};


#[test]
fn student_example() {

    fn mk_hm(version: i32) -> HashMap<Arc<str>, u32> {
        let mut hm = HashMap::new();
        if version == 0 {
            hm.insert("pretzels".into(), 15);
            hm.insert("chips".into(), 10);
            hm.insert("popcorn".into(), 10);
        } else if version == 1 {
            hm.insert("pretzels".into(), 12);
            hm.insert("chips".into(), 7);
            hm.insert("popcorn".into(), 6);
        } else if version == 2 {
            hm.insert("candy".into(), 5);
            hm.insert("chips".into(), 10);
            hm.insert("popcorn".into(), 25);
        }
        hm
    }


#[test]
fn student_test1() {
    let hm = mk_hm(0);
    let fsm = Fsm::create(&hm);
    let mut actions1 = Vec::new();
    actions1.push(VendingAction::Insert(27));
    actions1.push(VendingAction::Vend("pretzels".into()));

    let mut actions2 = Vec::new();
    actions2.push(VendingAction::Insert(10));
    actions2.push(VendingAction::Vend("chips".into()));

    let mut actions3 = Vec::new();
    actions3.push(VendingAction::Insert(5));
    actions3.push(VendingAction::Insert(5));
    actions3.push(VendingAction::Vend("popcorn".into()));

    assert!(fsm.eval(&actions1));
    assert!(fsm.eval(&actions2));
    assert!(fsm.eval(&actions3));
}
}
