use std::collections::HashMap;

///        ()          =>       {}
///    Matcher              Transcriber
/// match for patterns   expand to code
///
/// Rust 将尝试匹配 匹配器 中定义的模式，可以是 (), {} 或 []。
/// 宏规则中的字符将与输入匹配，以确定它是否匹配。
/// 在规则的模式成功匹配后，我们可以捕获模式的一部分
/// 并将其捕获为变量以在转录器中使用。
/// 美元符号后面的第一部分是变量的名称，它将在转录器中作为变量使用。
/// 分号后面的最后一部分称为指示符，它们是我们可以选择的匹配的类型。
///    ($name : expr)  告诉 Rust匹配表达式，并将其捕获为 $name.
///
///
// Rust中可用的指示符的列表:
// block: 一个块 (即一个语句块和/或一个表达式，用大括号括起来)
// stmt: 一份声明
// pat: 一种模式
// expr: 一个表达式
// ty: 一种类型
// ident: 标识符
// path: 路径 ( 如 foo, ::std::mem::replace)
// meta: 元项目；进入 #[...] 和 #![...] 属性的东西
// tt: 单个令牌树
#[macro_export]
macro_rules! yo {
    ($name: expr) => {
        println!("Yo {}", $name);
    };
}

#[macro_export]
macro_rules! hash_map {
    (
        $key: expr => $value:expr
    ) => {
        println!("{} {}", $key, $value);
    };
}

///   ( $(  $x: expr ), *)
/// '*' repeats patterns inside $(...);
/// ',' is now a separator.
#[macro_export]
macro_rules! hash_map2 {
    (
        $( $key: expr => $value:expr),*
    ) => {
        {
            let mut hm = HashMap::new();
            // '$( )*' 意味着此代码将针对每个重复层进行专门扩展
            $(hm.insert($key, $value); )*
            hm
        }
    }
}

fn main() {
    yo!("github");

    hash_map!("key1" => "value1");
    let user = hash_map2!("name" => "Finn", "gener" => "Boy");
    println!("User {:?}", user);

}
