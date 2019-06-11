use hello_macro::HelloMacro;
use hello_macro_derive::HelloMacro;

// 'derive' only works for structs and enums
#[derive(HelloMacro)]
struct Pancakes;

fn main() {
    Pancakes::hello_macro();
}
