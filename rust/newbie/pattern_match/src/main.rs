fn match_by_for_loop() {
    let v = vec!['a', 'b', 'c'];

    // enumerate used to adapt an iterator to produce a value and that value's index in the
    // iterator, placed into a tuple, like (0, 'a'). It matches the pattern '(idx, value)'
    for (idx, value) in v.iter().enumerate() {
        println!("{} is at index {}", value, idx);
    }
}

fn match_by_while_let() {
    let mut stack = Vec::new();
    stack.push(1);
    stack.push(2);
    stack.push(3);

    while let Some(top) = stack.pop() {
        println!("{}", top);
    }
}
// downside of using 'if let' is that the compiler doesn't check exhaustiveness, 
// but 'match' does.
fn match_by_if_let() {
    let favoriate_color: Option<&str> = Some("Green");
    let is_tuesday = false;
    let age: Result<u8, _> = "34".parse();

    if let Some(color) = favoriate_color {
        println!("Using your favorite color, {}, as the background", color);
    } else if is_tuesday {
        println!("Tuesday is green day!");
    } else if let Ok(age) = age {
        if age > 30 {
            println!("Using purple as the background color");
        } else {
            println!("Using orange as the background color");
        }
    } else {
        println!("Using blue as the background color");
    }
}

fn match_by_let() {
    // let PATTERN = EXPRESSION;
    // variable name in the 'PATTERN' slot, it's just a particular simple form of a pattern.
    // 'x' is a pattern that means "bind what matches here to the variable 'x'".
    let _x = 5;

    // use a pattern with 'let' to destruct a tuple.
    // Rust binds '1' to 'x', '2' to 'y', and '3' to 'z'.
    // Think of this pattern as nesting three individual variable patterns inside it.
    let (_x, _y, _z) = (1, 2, 3);
}

// -------------------
// &(3,5) matches pattern &(x,y)
fn foo(&(x, y) : &(i32, i32)) {
    println!("Current location: ({}, {})", x, y);
}

fn match_by_fn() {
    let point = (3, 5);
    foo(&point);
}

fn basic_match() {
    match_by_if_let();
    match_by_while_let();
    match_by_let();
    match_by_for_loop();
    match_by_fn();
}

// =================================
struct Point {
    x: i32,
    y: i32,
}

enum Color {
    Rgb(i32, i32, i32),
    Hsv(i32, i32, i32)
}

enum Message {
    Quit,
    Move {x: i32, y: i32},
    Write(String),
    ChangeColor(Color),
}

fn match_to_destruct() {
    let p = Point{x:10, y:20};
    let Point{x:a, y:b} = p;
    assert_eq!(10, a);
    assert_eq!(20, b);

    // a shorthand for patterns that match struct fields
    let Point{x, y} = p;
    assert_eq!(10, x);
    assert_eq!(20, y);

    // separate 'Point' value into three cases by 'match'
    match p {
        Point{x, y:0} => println!("On the x axis at {}", x),
        Point{x:0, y} => println!("On the y axis at {}", y),
        Point{x, y} => println!("On the neither axis: ({}, {})", x, y),
    }

    // destruct nested struct and enum
    let msg = Message::ChangeColor(Color::Hsv(0, 22, 33));
    //let msg = Message::Quit;
    match msg {
        Message::Quit => {
            println!("The quit variant has no data to destructure");
        },
        Message::Move{x, y} => {
            println!("Move in the x direction {} and in the y direction {}", x, y);
        },
        Message::Write(text) => {
            println!("Text message: {}", text);
        },
        Message::ChangeColor(color) => {
            match color {
                Color::Rgb(r, g, b) => {
                    println!("Change the color to ({}, {}, {})", r, g, b);
                },
                _ => {
                    println!("Change the color to other.");
                }
            }
        },
    }
}

fn match_range() {
    let x = 5;
    match x {
        1...5 => println!("one through five"),
        _ => println!("something else"),
    }

    let y = 'c';
    match y {
        'a'...'d' => println!("a->d"),
        _ => println!("other things"),
    }
}

fn multiple_patterns() {
    let x = 1;
    match x {
        1|2 => println!("one or two"),
        3 => println!("three"),
        _ => println!("anything"),
    }
}

fn match_ignore() {
    let mut m_value = Some(5);
    let f_value = Some(10);
    match (m_value, f_value) {
        (Some(_), Some(_)) => {
            println!("Can't overwrite an existing customized value");
        }
        _ => {
            m_value = f_value;
        }
    }
    println!("Set value: {:?}", m_value);

    let x = Some(String::from("hello"));
    match x {
        Some(_x) => { println!("Get x: {}", _x)},
        _ => {}
    }
    // _x会绑定值,转移了所有权,此处再用x会报错
    //println!("{:?}", x);    

    let p = Point{x:0, y:20};
    match p {
        // use '..' to ignore remaining part of a value
        Point{x, ..} => println!("x is {}", x),
    }

    let numbers = (2, 4, 3, 8, 2, 9);
    match numbers {
        (first, .., last) => {
            println!("Some numbers: {}, .., {}", first, last);
        }
    }
}

fn match_guard() {
    let num = Some(8);
    match num {
        Some(x) if x < 5 => { println!("less than five: {}", x); }
        Some(x) => println!("x: {}", x),
        None => {},
    }

    let x = Some(5);
    let y = 10;
    match x {
        Some(50) => println!("Got 50"),
        Some(n) if n == y  => println!("Matched. n: {:?}", n),
        _ => println!("Default case, x:{:?}", x),
    }
}


fn match_at_binding() {
    enum Message2 {
        Hello { id: i32},
    }

    let msg = Message2::Hello{ id: 5};
     match msg {
         // 加入 'id_var@' 捕获了值
         Message2::Hello{ id: id_var @ 3...7 } => {
             println!("Found an id in range: {}", id_var);
         }
         // 未包含实际的变量
         Message2::Hello{ id: 10...12 } => {
             println!("Found an id in another range");
         }
         // 未指定range,匹配其它值赋给id
         Message2::Hello{ id } => {
             println!("Found some other id: {}", id)
         }
     }
}

fn patterns() {
    multiple_patterns();
    match_range();
    match_to_destruct();
    match_ignore();
    match_guard();
    match_at_binding();
}

// =================================

fn main() {
    basic_match();

    patterns();
}
