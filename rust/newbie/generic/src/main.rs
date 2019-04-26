use std::fmt::Display;
// --------------- Generic Types ----------------

fn largest<T: PartialOrd + Copy>(list: &[T]) -> T {
    // 这里用到了Copy特性,所以需要在T中加上此特性,以支持move
    let mut largest = list[0];

    for &item in list.iter() {
        if item > largest {
            largest = item;
        }
    }
    largest
}

/*
fn largest2<T: PartialOrd>(list: &[T]) -> &T {
    // 这里用到了Copy特性,所以需要在T中加上此特性,以支持move
    let mut largest = &list[0];

    for &item in list.iter() {
        if item > (*largest) {
            largest = &item;
        }
    }
    largest
}
*/

struct Point<T> {
    x : T,
    y : T,
}

// only Point<f32> has function of 'p'
impl Point<f32> {
    fn p(&self) {
        println!("Point is: {}, {}", self.x, self.y);
    }
}

impl<T: Display + PartialOrd> Point<T> {
    fn cmp_display(&self) {
        // 比较需要支持 'PartialOrd' 特性
        if self.x > self.y {
            // 打印需要支持 'Display' 特性
            println!("The larger member is x = {}", self.x);
        } else {
            println!("The larger member is y = {}", self.y);
        }
    }
}

struct Point2<T, U> {
    x : T,
    y : U,
}

impl<T, U> Point2<T, U> {
    fn x(&self) -> &T {
        &self.x
    }

    fn y(&self) -> &U {
        &self.y
    }

    // mix
    fn mixup<V, W>(self, other: Point2<V, W>) -> Point2<T, W> {
        Point2 {
            x: self.x,
            y: other.y,
        }
    }
}

// --------------- Traits ----------------
pub trait Summary {
    fn summarize_author(&self) -> String;

    fn summarize(&self) -> String {
        format!("Read more from {}...", self.summarize_author())
    }
}

struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

// 采用默认实现
impl Summary for NewsArticle { 
    fn summarize_author(&self) -> String {
        String::from("NewsArticle author")
    }
}

struct Tweet {
    username: String,
    content: String,
    reply: bool,
    retweet: bool,
}
impl Summary for Tweet {
    fn summarize_author(&self) -> String {
        String::from("Tweet author")
    }

    fn summarize(&self) -> String {
        format!("{}: {}", self.username, self.content)
    }
}

// Trait作为参数
fn notify(item: impl Summary) {
    println!("Breaking news: {}", item.summarize());
}

// ----------------------------- lifetime -------------------------
// lifetime 
// 签名中的所有引用都有相同的生命周期a
// 错误的函数签名, Rust不知道返回的是x还是y的引用
//fn longest(x: &str, y: & str) -> & str {
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

struct ImportantExcerpt<'a> {
    part: &'a str,
}

// 带泛型参数、Trait Bounds和 lifetime
fn longest_with_an_announcement<'a, T>(x: &'a str, y: &'a str, ann: T) -> &'a str
    where T: Display
{
    println!("Announcement! {}", ann);
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

fn main() {

    let number_list = vec![34, 50, 25, 100, 33];
    let result = largest(&number_list);
//    let result = largest2(&number_list);
    println!("largest number is {}", result);

    let char_list = vec!['y', 'm', 'a', 'd'];
    let result = largest(&char_list);
    println!("larget char is {}", result);


    let p1 = Point{x: 1, y: 2};
    let p2 = Point{x: 1.0, y: 2.0};
    // Compile error. p() only defined within Point<f32>, not Point<i32>
//    p1.p();
    p2.p();
    p1.cmp_display();

    let p3 = Point2{x:1.0, y:2};
    println!("p3: {} {}", p3.x(), p3.y());

    let p4 = Point2{x:8.0, y:"ok"};
    let p5 = p3.mixup(p4);
    println!("p5.x:{}, p5.y:{}", p5.x, p5.y);

    //---------------- Traits ------------------
    let tweet = Tweet {
        username: String::from("horse"),
        content: String::from("of course, as you probably already know, people"),
        reply: false,
        retweet: false,
    };

    notify(tweet);

    let article = NewsArticle {
        headline: String::from("Penguins win the Stanley Cup Championship!"),
        location: String::from("Pittsburgh, PA, USA"),
        author: String::from("Iceburgh"),
        content: String::from("The Pittsburgh Penguins once again are the best hockey team in the NHL."),
    };

    notify(article);

    let str1 = "short";
    let str2 = "very long";
    println!("longer string is : {}", longest(str1, str2));

    // ---------------- lifetime test ---------------
    let novel = String::from("Call me maybe. Some years ago ....");
    let first_sentence = novel.split(".").next().expect("Could not find a '.'");
    let i = ImportantExcerpt{ part: first_sentence};

    let s : &'static str = "I have a static lifetime";

    longest_with_an_announcement(str1, str2, "GO");
}
