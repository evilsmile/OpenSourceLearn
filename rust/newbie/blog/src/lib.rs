//! 只例用于说明 Rust 的OOP编程模式
//!
// 博客发表顺序：
//  1. 草稿状态  <Draft>      此时去获取内容是空的，因为未发布
//  2. Review状态 <Review>    此时去获取内容是空的，因为未发布
//  3. 发布状态  <Publish>    此时去获取内容才是编辑的内容
//
//  借助于状态机，我们避免使用match相关的大量arm代码。
//  而且扩展方便。
//
//  扩展：该功能实现的另一个思路，参见 ../blog2/
//        新思路虽然不那么OOP，但是把状态编到代码里，
//        一些违规操作可以在编译期就被发现。挺有意思的


pub struct Post {
    state: Option<Box<dyn State>>,           // Post实现的操作都进一步由state相应操作来完成。它在运行中会更换，像多态一样效果
    content: String,
}

impl Post {
    pub fn new() -> Post {
        Post{
            state: Some(Box::new(Draft{})),
            content: String::new(),
        } 
    }

    pub fn add_text(&mut self, text: &str) {
        self.content.push_str(text);
    }

    pub fn request_review(&mut self) {
        // take 从Option中取值,并在原处留下None
        // take 使得接管了所有权,而不是borrow
        // 之所以不直接设置 self.state = self.state.request_review()，
        // 是因为这样可以保证Post转移到新状态后不会再使用旧的state值
        if let Some(s) = self.state.take() {
            self.state = Some(s.request_review())
        } 
    }

    pub fn approve(&mut self) {
        if let Some(s) = self.state.take() {
            self.state = Some(s.approve())
        }
    
    }

    pub fn content(&self) -> &str {
        // 调用 as_ref() 只引用值而不拥有值: 返回 Option<&Box<dyn State>>
        self.state.as_ref().unwrap().content(&self)
    }
}

trait State {
    // 'Self' trait
    // 'self: Box<Self>' 表明只有在调用Box持有类型时 request_review方法才合法
    fn request_review(self: Box<Self>) -> Box<dyn State>;

    fn approve(self: Box<Self>) -> Box<dyn State>;

    fn content<'a>(&self, post: &'a Post) -> &'a str {
        ""
    }
}

struct Draft {}

impl State for Draft {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        // 返回一个新的 Box 封装的实例
        Box::new(PendingReview{})
    }

    fn approve(self: Box<Self>) -> Box<dyn State> {
        self
    }
}

struct PendingReview {}

impl State for PendingReview {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        self
    }

    fn approve(self: Box<Self>) -> Box<dyn State> {
        Box::new(Published{})
    }
}

struct Published {}

impl State for Published {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        self
    }

    fn approve(self: Box<Self>) -> Box<dyn State> {
        self
    }

    fn content<'a>(&self, post: &'a Post) -> &'a str {
        &post.content
    }
}
