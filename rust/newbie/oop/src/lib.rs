// -------------- trait object 实现 ---------------
pub trait Draw {
    fn draw(&self);
} 

pub struct Screen {
    pub components : Vec<Box<dyn Draw>>,
}

impl Screen {
    pub fn run(&self) {
        for component in self.components.iter() {
            component.draw();
        }
    }
}

pub struct Button {
    pub width : u32,
    pub height: u32,
    pub label: String,
}

impl Draw for Button {
    fn draw(&self) {
        println!("Button[{}] drawed!", self.label);
    }
}

// --------- 对比使用 generic type parameter with trait bounds ------
// 这里指定了T的类型,所以只能替换某一种具体类型
// 但是上面的 trait object 可以替换多种具体类型
pub struct Screen2<T: Draw> {
    pub components : Vec<T>,
}

impl<T> Screen2<T> 
where T: Draw {
    pub fn run(&self) {
        for component in self.components.iter() {
            component.draw();
        }
    }
}
