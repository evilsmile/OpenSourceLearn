// -------------- Box<T> ------------
mod boxlist {
    pub enum List {
        Cons(i32, Box<List>),
        Nil,
    }
}
use crate::boxlist::List::{Cons, Nil};

//--------------------- Deref trait ------
use std::ops::Deref;
struct MyBox<T>(T);

impl<T> MyBox<T> {
    fn new(x: T) -> MyBox<T> {
        MyBox(x)
    }
}

impl<T> Deref for MyBox<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}

fn hello(name: &str) {
    println!("Hello, {}", name);
}

// --------------- Drop trait -------------
struct CustomSmartPointer {
    data: String,
}
impl Drop for CustomSmartPointer {
    fn drop(&mut self) {
        println!("Dropping CustomSmartPointer with data `{}`!", self.data);
    }
}

// -------------- Rc + RefCell -----------
mod rclist {
    use std::rc::Rc;
    use std::cell::RefCell;
    #[derive(Debug)]
    pub enum List {
        Cons(Rc<RefCell<i32>>, Rc<List>),
        Nil
    }
}
use crate::rclist::List::Cons as Cons2;
use crate::rclist::List::Nil as Nil2;
use std::rc::Rc;
use std::cell::RefCell;

// ------------- Cycle Ref -------------
mod cyclelist {
    use std::rc::Rc;
    use std::cell::RefCell;

    #[derive(Debug)]
    pub enum List {
        Cons(i32, RefCell<Rc<List>>),        // 修改的是Cons中的下一个List
        Nil,
    }
    impl List {
        pub fn tail(&self) -> Option<&RefCell<Rc<List>>> {
            match self {
                List::Cons(_, item) => Some(item),
                List::Nil => None,
            }
        }
    }
}

use crate::cyclelist::List::Cons as Cons3;
use crate::cyclelist::List::Nil as Nil3;

fn test_cycle_ref() {
    println!("------- ref cycle test -----");

    let a = Rc::new(Cons3(5, RefCell::new(Rc::new(Nil3))));
    println!("a initial rc count = {}", Rc::strong_count(&a));
    println!("a next item = {:?}", a.tail());

    let b = Rc::new(Cons3(9, RefCell::new(Rc::clone(&a))));
    println!("a rc count after b creation = {}", Rc::strong_count(&a));
    println!("b inital rc count = {}", Rc::strong_count(&b));
    println!("b next item = {:?}", b.tail());

    //    a(#5#List) -----> b(#9#List)
    //     /|\                    |
    //      |----------------------      [now cycle ref]
    if let Some(link) = a.tail() {
        *link.borrow_mut() = Rc::clone(&b);
    }
    println!("a initial rc count = {}", Rc::strong_count(&a));
    println!("b inital rc count = {}", Rc::strong_count(&b));

    // 无穷循环打印
    //println!("a next item = {:?}", a.tail());
    println!("------- ref cycle test End -----");
}

// ------------ Weak Ref ------------------
#[derive(Debug)]
struct Node {
    value: i32,
    children: RefCell<Vec<Rc<Node>>>,   // strong ref: 父节点删除后子节点也应该删除
    parent: RefCell<Weak<Node>>,        // weak ref: 子节点删除后父节点还在. 避免循环引用
}

use std::rc::Weak;
fn test_weak_ref() {
    let leaf = Rc::new(Node{
        value: 3,
        parent: RefCell::new(Weak::new()),
        children: RefCell::new(vec![]),
    });

    println!("leaf parent = {:?}", leaf.parent.borrow().upgrade());
    println!("leaf strong = {}, weak = {}", Rc::strong_count(&leaf), Rc::weak_count(&leaf));

    let branch = Rc::new(Node{
        value: 5,
        parent: RefCell::new(Weak::new()),
        children: RefCell::new(vec![Rc::clone(&leaf)]),
    });
    // downgrade 创建了一个 'Weak<Node>' 引用
    *leaf.parent.borrow_mut() = Rc::downgrade(&branch);
    // 此处不会有无限打印, 只会打印出 '(Weak)'
    println!("leaf parent = {:#?}", leaf.parent.borrow().upgrade());

    println!("leaf strong = {}, weak = {}", Rc::strong_count(&leaf), Rc::weak_count(&leaf));
    println!("branch strong = {}, weak = {}", Rc::strong_count(&branch), Rc::weak_count(&branch));
}

// -------------- main -------------
fn main() {
    let list = Cons(1,
       Box::new(Cons(2,
         Box::new(Cons(3,
           Box::new(Nil))))));

    let x = 5;
    let y = MyBox::new(x);
    assert_eq!(5, *y);

    // ------- Deref --------
    let m = MyBox::new(String::from("Rust"));
    hello(&m);

    // ------ Drop ------
    let c = CustomSmartPointer { data: String::from("my stuff") };
    drop(c);
    println!("CustomSmartPointer dropped early!");
    let d = CustomSmartPointer { data: String::from("other stuff") };
    println!("CustomSmartPointer created!");

    // ---- Rc + RefCell 使得内容可以修改 ----
    let value = Rc::new(RefCell::new(5));
    let list2_1 = Rc::new(Cons2(Rc::clone(&value), Rc::new(Cons2(Rc::new(RefCell::new(10)), Rc::new(Nil2)))));

    // *value是 RefCell的,可以借用为可修改的
    *value.borrow_mut() += 1000;
    println!("list2's count after 1: strong: {} weak: {}", Rc::strong_count(&list2_1), Rc::weak_count(&list2_1));
    {
        let list2_2 = Cons2(Rc::new(RefCell::new(3)), Rc::clone(&list2_1));
        println!("list2's count after 2: strong: {} weak: {}", Rc::strong_count(&list2_1), Rc::weak_count(&list2_1));
        println!("list2_2 = {:?}", list2_2);
    }
    println!("list2's count before 2: strong: {} weak: {}", Rc::strong_count(&list2_1), Rc::weak_count(&list2_1));
    let list2_3 = Cons2(Rc::new(RefCell::new(4)), Rc::clone(&list2_1));
    println!("list2's count after 3: strong: {} weak: {}", Rc::strong_count(&list2_1), Rc::weak_count(&list2_1));

    println!("list2_1 = {:?}", list2_1);
    println!("list2_3 = {:?}", list2_3);

    test_cycle_ref();
    test_weak_ref();
}
