mod ac {
    pub struct AveragedCollection {
        list: Vec<i32>,
        average: f64,
    }

    impl AveragedCollection {
        pub fn new() -> AveragedCollection {
            AveragedCollection{
                list: vec![],
                average: 0.0,
            }
        }

        pub fn add(&mut self, value: i32) {
            self.list.push(value);
            self.update_average();
        }

        pub fn remove(&mut self) -> Option<i32> {
            let result = self.list.pop();
            match result {
                Some(value) => {
                    self.update_average();
                    Some(value)
                },
                None => None,
            }
        }

        pub fn average(&self) -> f64 {
            self.average
        }

        fn update_average(&mut self) {
            let total : i32 = self.list.iter().sum();
            self.average = total as f64 / self.list.len() as f64;
        }
    }
}

// -------------------------------
use oop::{Draw, Screen, Button};
struct SelectBox {
    width: u32,
    height: u32,
    options: Vec<String>,
}
impl Draw for SelectBox {
    fn draw(&self) {
        println!("Select-Options: ");
        for op in self.options.iter() {
            println!("-> {}", op);
        }
    }
}

fn test_gui_trait_object() {
    let mut scr = Screen{components: vec![]};

    let sb = SelectBox{ 
        width: 20, 
        height: 32, 
        options: vec![
            String::from("yes"),
            String::from("no"),
        ]
    };

    let bt = Button {
        width: 30,
        height: 40,
        label: String::from("Checkout!"),
    };

    scr.components.push(Box::new(sb));
    scr.components.push(Box::new(bt));
    scr.run();
}

fn main() {
    let mut ac = ac::AveragedCollection::new();
    ac.add(10);
    ac.add(20);
    ac.add(30);
    ac.add(33);
    println!("Aver: {}", ac.average());
    ac.remove();
    println!("Aver: {}", ac.average());

    test_gui_trait_object();
}
