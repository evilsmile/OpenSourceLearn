//!  此例是 blog/ 的另一种实现，不采用状态机，而是直接利用Rust的type system在编译期就避免错误。
//!   在编译期就可以发现'显示还未发布的Post'等bug
///
///  业务逻辑是这样的：
///    blog的发布过程： Draft -> [request_review] -> PendingReview -> [approve] -> Post
///     只有在到了Post阶段才能返回content，因为其它阶段还未发布。
///
///  通过返回值进入下一阶段
///
pub struct DraftPost {
    content: String,
}

impl DraftPost {
    pub fn request_review(self) -> PendingReviewPost {
        PendingReviewPost {
            content: self.content
        }
    }

    pub fn add_text(&mut self, text: &str) {
        self.content.push_str(text);
    }
}

pub struct PendingReviewPost {
    content: String,
}

impl PendingReviewPost {
    pub fn approve(self) -> Post {
       Post {
           content: self.content
       }
    }
}

pub struct Post {
    content: String,
}

impl Post {
    pub fn new() -> DraftPost {
        DraftPost {
            content: String::new(),
        }
    }

    pub fn content(&self) -> &str {
        &self.content
    }
}
