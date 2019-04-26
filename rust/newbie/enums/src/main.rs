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

fn main() {
    let m = Message::Write(String::from("hello"));
    m.call();

    let some_num = Some(5);
    let some_str = Some("a string");
    let absent_num : Option<i32> = None;

    let coin = Coin::Quarter(UsState::Alabama);

    value_in_coin(coin);
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
