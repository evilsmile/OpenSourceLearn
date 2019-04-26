use std::collections::HashMap;

fn main() {
    let teams = vec![String::from("Blue"), String::from("Yellow")];
    let initial_scores = vec![10, 50];

    // use zip to create a vector of tuples
    let scores : HashMap<_, _> = teams.iter().zip(initial_scores.iter()).collect();

    println!("{:#?}", teams.iter());
    println!("{:#?}", initial_scores.iter());
    println!("{:#?}", teams.iter().zip(initial_scores.iter()));
    println!("{:#?}", scores);

    for (k, v) in &scores {
        println!("{}: {}", k, v);
    }

    let mut scores2  = HashMap::new();
    // Compile ERROR! hash map类型要一致
    //scores2.entry(23).or_insert(800);
    scores2.insert(String::from("Blue"), 10);
    scores2.entry(String::from("Blue")).or_insert(100);
    scores2.entry(String::from("Black")).or_insert(800);
    println!("{:#?}", scores2);

    let text = "Hello world wonderful world";
    let mut map = HashMap::new();

    for word in text.split_whitespace() {
        let count = map.entry(word).or_insert(0);
        *count += 1;
    }
    println!("{:?}", map);
}
