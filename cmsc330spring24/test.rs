extern crate regex;
use regex::Regex;

fn main() {
    println!("Try programiz.pro");
    let example = "123 + 456 - 789";
    let tokens = lex(example);
    for token in tokens {
        println!("{}", token);
    }
}

fn lex(sentence: &str) -> Vec<&str> {
    let mut vec = Vec::new();
    let add = Regex::new(r"^\+").unwrap();
    let sub = Regex::new(r"^\-").unwrap();
    let num = Regex::new(r"^\d+").unwrap();
    
    for ch in sentence.chars() {
        let str = ch.to_string(); // Convert char to string for regex matching

        if num.is_match(&str) {
            vec.push("Number");
        } else if add.is_match(&str) {
            vec.push("Add");
        } else if sub.is_match(&str) {
            vec.push("Sub");
        } else if str == " " {
            continue;
        }
    }

    vec
}
