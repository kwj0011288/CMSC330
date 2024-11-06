//! This is the file where you will write your code.
#![allow(unused_imports)]

use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
    iter,
    ops::Range,
};

use regex::Regex;

/// Sums the numbers from 1 to `n`, where `n` is an arbitrary positive
/// integer.
/// # Example
///
/// ```
/// # use project_6::gauss;
/// assert_eq!(gauss(2), Some(3));
/// ```

pub fn gauss(n: i32) -> Option<i32> {
    let mut sum = 0;
    if n <= 0 {
        return None;
    } else {

      for i in 1..= n {
        sum += i;
      }

    }
    return Some(sum);
}

/// This function takes in a slice of integers and returns a vec of integers
/// The returned vec is double the slice but with a twist
/// It doubles up and dances!

pub fn double_up_and_dance(slice: &[i32]) -> Vec<i32> {
    let mut vec = Vec :: new();

    for item in slice {
        vec.push(*item);
        vec.push(*item);
    }

    if vec.len() >= 5 {
        vec[4] += slice[1];
    }
    return vec
    
}

/// This function takes in an integer and returns the equivalent binary string

pub fn to_binstring(num: u32) -> String {
     let mut string = String :: new();
     let mut value = num;

     if value == 0 {
        string.push('0');
        println!("{}", string);
        return string 
     } else {
        while value > 0 {
             if value % 2 == 0 {
                string.push('0');
            } else {
                string.push('1');
            }
            value = value / 2
        }
     }
     return string.chars().rev().collect()

}

/// This function takes in an iterable of integers,
/// and counts how many elements in the iterator are within the given range

pub fn in_range(items: impl IntoIterator<Item = i32>, range: Range<i32>) -> usize {
    //start include, end exclude
    let mut num = 0;

    for item in items {
        if range.contains(&item) {
            num += 1;
        }
    }
    return num
}

/// Given an iterator over mutable strings, this function will capitalize the
/// first letter of each word in the iterator, in place.

pub fn capitalize_words<'a>(words: impl IntoIterator<Item = &'a mut String>) {
    
    
    for word in words {
        let mut chars = word.chars();
        if let Some(first) = chars.next() {
            *word = first.to_uppercase().chain(chars).collect();
        }
    }
}

/// Given a txt file, parse and return the items sold by a vending machine, along
/// with their prices.

pub fn read_prices(filename: &str) -> Option<HashMap<String, u32>> {
    let file =  File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let re = Regex::new(r"^[A-Za-z ]+; *\d+ *(c|cents)?$").unwrap();
    let mut items = HashMap::new();

    for line in reader.lines() {
        let line = line.unwrap().trim().to_string();

        if line.starts_with(';') || line.is_empty() {
            continue;
        }
        
        if !re.is_match(&line) {
            return None;
        }

        let find_semicolon = line.find(';');
        let semicolon = ";";

        let item = line[0..find_semicolon.unwrap()].trim().to_string();
        let temp = line[find_semicolon.unwrap() + 1..].replace("cents", "").replace("c", "");
        let price = temp.trim().parse().unwrap();

        if price < 1 || price > 50 || items.contains_key(&item) {
            return None;
        } else {
            items.insert(item, price);
        }
    }
    Some(items)
}



#[cfg(test)]
mod tests {
    use super::*;

    // STUDENT TESTS:
    #[test]
    fn student_test1() {
        assert!(true)
    }

    // PUBLIC TESTS:

    #[test]
    fn test_gauss() {
        assert_eq!(gauss(1), Some(1));
        assert_eq!(gauss(5), Some(15));
        assert_eq!(gauss(10), Some(55));
        assert_eq!(gauss(19), Some(190));
    }

    #[test]
    fn test_gauss2() {
        assert_eq!(gauss(-2), None);
        assert_eq!(gauss(-400), None);
        assert_eq!(gauss(330), Some(54615));
        assert_eq!(gauss(0), None);
    }

    #[test]
    fn test_double_up_and_dance() {
        assert_eq!(double_up_and_dance(&[-9, 7]), [-9, -9, 7, 7]);
        assert_eq!(double_up_and_dance(&[]), []);
    }

    #[test]
    fn test_double_up_and_dance2() {
        assert_eq!(double_up_and_dance(&[1, 2, 3]), [1, 1, 2, 2, 5, 3]);
        assert_eq!(
            double_up_and_dance(&[1, -2, 3, 8]),
            [1, 1, -2, -2, 1, 3, 8, 8]
        );
    }

    #[test]
    fn test_in_range1() {
        assert_eq!(in_range([5, 2, 1, 3, 9], 2..6), 3);
        assert_eq!(in_range([5, 2, 1, 3, 9], 3..5), 1);
        assert_eq!(in_range(vec![5, 2, 1, 3, 9], 2..11), 4);
        assert_eq!(in_range([1, 3, 5], 2..5), 1);
        assert_eq!(in_range([1, 2, 3, 5, 6], 2..5), 2);
    }

    #[test]
    fn test_in_range2() {
        assert_eq!(in_range([], 2..11), 0); // check empty handling
        assert_eq!(in_range([-4, -3, -2, -1, 0, 1, 2, 3, 4], -3..3), 6); // ensure negatives handled
        assert_eq!(in_range(0..100, 25..51), 26); // larger range check
    }

    #[test]
    fn test_to_binstring1() {
        assert_eq!(to_binstring(1), "1",);
        assert_eq!(to_binstring(2), "10");
        assert_eq!(to_binstring(3), "11");
        assert_eq!(to_binstring(6), "110");
        assert_eq!(to_binstring(9), "1001");
        assert_eq!(to_binstring(32), "100000");
        assert_eq!(to_binstring(35), "100011");
    }

    #[test]
    fn test_to_binstring2() {
        assert_eq!(to_binstring(0), "0"); // special case of 0
        assert_eq!(to_binstring(511), "111111111");
        assert_eq!(to_binstring(900), "1110000100");
        assert_eq!(to_binstring(1024), "10000000000");
        assert_eq!(to_binstring(1099), "10001001011");
        assert_eq!(to_binstring(41952), "1010001111100000");
        assert_eq!(to_binstring(76351), "10010101000111111");
    }

    #[test]
    fn test_capitalize_words() {
        // Testing modifying in place

        let mut tester = vec!["cmsc330".to_owned(), "is".to_owned(), "great!".to_owned()];
        capitalize_words(&mut tester);
        let expected = ["Cmsc330".to_owned(), "Is".to_owned(), "Great!".to_owned()];
        assert_eq!(tester, expected);

        let mut tester2 = ["hello".to_owned(), "world".to_owned()];
        capitalize_words(&mut tester2);
        let expected2 = ["Hello".to_owned(), "World".to_owned()];
        assert_eq!(tester2, expected2);
    }

    #[test]
    fn test_capitalize_words_empty() {
        let mut tester3 = [];
        capitalize_words(&mut tester3);
        let expected3: [String; 0] = [];
        assert_eq!(tester3, expected3);

        capitalize_words(iter::empty());
    }

    #[test]
    fn test_read_prices_valid() {
        assert_eq!(
            read_prices("inputs/file1.txt"),
            Some(HashMap::from([
                ("ice cream".to_owned(), 10),
                ("sandwich".to_owned(), 49),
                ("hot dog".to_owned(), 49)
            ]))
        );

        assert_eq!(read_prices("inputs/empty.txt"), Some(HashMap::new()));
    }

    #[test]
    fn test_read_prices_invalid() {
        assert_eq!(read_prices("inputs/file2.txt"), None);
        assert_eq!(read_prices("inputs/file3.txt"), None);
        assert_eq!(read_prices("inputs/file4.txt"), None);
        assert_eq!(read_prices("inputs/file5.txt"), None);
    }
}
