#![allow(unused_variables)]

#[derive(Debug)]
enum Message {
    Quit,
    Move { x: i32, y: i32},
    Write(String),
    ChangeColor(i32, i32, i32),
}

impl Message {
    fn call(&self) {
        println!("Message: {:#?}", self);
    }
}

#[derive(Debug)]
enum UsState {
    Alabama,
    Alaska,
}

enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter(UsState),
}

fn test_while_loop() {
    let mut stack = Vec::new();
    stack.push(1);
    stack.push(2);
    stack.push(3);

    while let Some(top) = stack.pop() {
        println!("{}", top);
    }
}

fn main() {
    let m = Message::Write(String::from("hello"));
    m.call();

    let some_num = Some(5);
    let some_str = Some("a string");
    let absent_num : Option<i32> = None;

    let coin = Coin::Quarter(UsState::Alabama);

    value_in_coin(coin);

    test_while_loop();
}
fn value_in_coin(coin: Coin) -> u8 {
    match coin {
        Coin::Penny => 1,
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter(state) => {
            println!("State is {:#?}", state);
            25
        }
    }
}
