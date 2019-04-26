use blog::Post;

fn main() {
    let mut post = Post::new();

    let content = "I ate a salad for lunch today";

    post.add_text(content);
    assert_eq!("", post.content());

    post.request_review();
    assert_eq!("", post.content());

    post.approve();
    assert_eq!(content, post.content());
}
