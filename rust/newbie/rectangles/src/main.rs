#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }
}

fn main() {
    let width = 12;
    let height = 13;

    let rect = Rectangle {
        width: width,
        height: height,
    };

    println!("The area of rectangle is {}", area(width, height));
    println!("The area of rectangle is {}", area2((width, height)));
    println!("The area of rectangle is {}", area3(&rect));
    println!("The area of rectangle is {}", rect.area());
    // Debug output
    println!("The Rectangle is {:?}", rect);
    println!("The Rectangle is {:#?}", rect);
}

fn area(w: u32, h: u32) -> u32 {
    w * h
}

fn area2(dimension: (u32, u32)) -> u32 {
    dimension.0 * dimension.1
}

fn area3(rect: &Rectangle) -> u32 {
    rect.width * rect.height
}
