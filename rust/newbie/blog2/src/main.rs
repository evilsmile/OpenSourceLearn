use blog2::*;

fn main() {
    let mut post = Post::new();
    post.add_text("What a good day!");

    post.content();
    let post = post.request_review();

    let post = post.approve();

    println!("content: {}", post.content());
}
