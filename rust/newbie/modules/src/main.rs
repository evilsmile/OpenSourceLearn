#![allow(unused_variables)]
mod sound;

mod plant {
    pub struct Vegetable {
        pub name: String,
        id: i32,
    }

    impl Vegetable {
        pub fn new(name: &str) -> Vegetable {
            Vegetable {
                name: String::from(name),
                id: 1
            }
        }
    }
}

fn main() {
    let mut veg = plant::Vegetable::new("squash");
    veg.name = String::from("new squash");

    println!("Vegetable: {}", veg.name);

    sound::instrument::clarinent();
}
