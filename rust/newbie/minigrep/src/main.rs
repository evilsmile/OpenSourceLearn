use std::env;
use std::process;

use minigrep::Config;

fn main() {
    // unwrap_or_else 在发生错误时调用closure 匿名函数
    let config = Config::new(env::args()).unwrap_or_else(|err| {
        // Error output
        eprintln!("Problem parsing arguments: {}", err);
        process::exit(1);
    });

    // 这里没有用unwrap_or_else因为并没值可unwrap，只关心Err
    if let Err(e) = minigrep::run(config) {
        eprintln!("Application error: {}", e);
        process::exit(1);
    }
}
