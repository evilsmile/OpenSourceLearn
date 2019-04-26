//! # minigrep
//!
//! `minigrep` is a simple implementation of grep, 
//! support case-sensitive/insensitive search
use std::error::Error;
use std::fs;
use std::env;

// struct成员可以从module内部访问，但不能由外部
pub struct Config {
    pub query: String,
    pub filename: String,
    pub case_sensitive: bool,
}

impl Config {
    // 使用 args的迭代特性
    pub fn new(mut args: std::env::Args) -> Result<Config, &'static str> {
        args.next();
        
        let query = match(args.next()) {
            Some(arg) => arg,
            None => return Err("Didn't get a query string"),
        };

        let filename = match(args.next()) {
            Some(arg) => arg,
            None => return Err("Didn't get a query string"),
        };

        // 读取环境变量，不关心值，只关心是否设置
        let case_sensitive = env::var("CASE_INSENSITIVE").is_err();

        Ok(Config{query, filename, case_sensitive})
    }
}

// trait object Box<dyn Error>, dyn short-for 'dynamic'
pub fn run(config: Config) -> Result<(), Box<dyn Error>>{

    let content = fs::read_to_string(config.filename)?;

    println!("Content:\n{}", content);

    let results = if config.case_sensitive {
        search(&config.query, &content)
    } else {
        search_case_insensitive(&config.query, &content)
    };

    for line in results {
        println!("{}", line);
    }

    Ok(())
}

/// look for specified string in content with case-sensitive way
/// # Examples
/// ```
///        let query = "duct";
///        let content = "\
///Rust:
///safe, fast, productive.
///Pick three.";
///        assert_eq!(
///            vec!["safe, fast, productive."],
///            minigrep::search(query, content)
///            );
/// ```
///
// 'a telling that lifetime of 'return' matches lifetime of 'content'
pub fn search<'a>(query: &str, content: &'a str) -> Vec<&'a str> {

    content.lines()
        .filter(|line| line.contains(query))
        .collect()
}

pub fn search_case_insensitive<'a>(query: &str, content: &'a str) -> Vec<&'a str> {
    let query = query.to_lowercase();
    let mut results = Vec::new();

    for line in content.lines() {
        if line.to_lowercase().contains(&query) {
            results.push(line);
        }
    }

    results
}

#[cfg(test)]
mod tests  {
    use super::*;

    #[test]
    fn case_sensitive() {
        let query = "duct";
        let content = "\
Rust:
safe, fast, productive.
Pick three.";
        assert_eq!(
            vec!["safe, fast, productive."],
            search(query, content)
            );
    }

    #[test]
    fn case_insensitive() {
        let query = "RUST";
        let content = "\
Rust:
safe, fast, productive.
Trust me.";
        assert_eq!(
            vec!["Rust:", "Trust me."],
            search_case_insensitive(query, content)
            );
    }
}
