#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() -> Result<(), String>{
        if 2 + 1 == 4 {
            Ok(())
        } else {
            Err(String::from("two plus two does not equal four"))
        }
    }

    #[test]
    fn it_down() {
        panic!("Make this panic!");
    }

    #[test]
    fn larger_can_hold_smaller() {
        let larger = Rectangle{width: 8, height: 7};
        let smaller = Rectangle{width: 5, height: 1};

        assert!(larger.can_hold(&smaller));
    }

    #[test]
    fn smaller_cannot_hold_large() {
        let larger = Rectangle{width: 8, height: 7};
        let smaller = Rectangle{width: 5, height: 1};

        assert!(!smaller.can_hold(&larger));
    }

    #[test]
    #[ignore]
    fn test_add() {
        assert_eq!(add_two(8), 10);
    }

    #[test]
    fn greeting_contains_name() {
        let result = greeting("Carl");
        assert!(result.contains("Carl"), 
                "Greeting did not contain name, value was `{}`", result);
    }

    #[test]
    #[should_panic(expected="Guess value must be less than or equal to 100")]
    // expected 中包含panic时应包含的子串
    fn greater_than_100() {
        Guess::new(200);
    }
}

struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}

pub fn add_two(a: i32) -> i32 {
    a + 2
}

pub fn greeting(user: &str) -> String {
    //format!("hello {}", user)
    String::from("Hello")
}

pub struct Guess {
    value: i32
}

impl Guess {
    pub fn new(value: i32) -> Guess {
        if value < 1 {
            panic!("Guess value must be greater or equal to 1, got {}.", value);
        } else if value > 100 {
            panic!("Guess value must be less than or equal to 100, got {}.", value);
        }
        Guess {
            value
        }
    }
}
