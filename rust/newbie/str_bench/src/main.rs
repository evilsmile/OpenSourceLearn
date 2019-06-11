use std::thread;
use std::time::{Duration, SystemTime};

fn main() {
    println!("Compare begin:");

    let sy_time0 = SystemTime::now();
    let n = 10000000;

    for _ in 0..n {
        let _x : String = "hello".to_string();
    }
    println!(
        "to_string:() => time :{} seconds",
        SystemTime::now()
        .duration_since(sy_time0)
        .unwrap()
        .as_secs()
        );

    let sy_time1 = SystemTime::now();
    for _ in 0..n {
        let _x : String = String::from("hello");
    }
    println!(
        "string::from() => time: {} seconds",
        SystemTime::now()
        .duration_since(sy_time1)
        .unwrap()
        .as_secs()
        );

    let sy_time2 = SystemTime::now();
    for _ in 0..n {
        let _x : String = "hello".into();
    }
    println!(
        "into() => time: {} seconds",
        SystemTime::now()
        .duration_since(sy_time2)
        .unwrap()
        .as_secs()
        );

    thread::sleep_ms(50000);
}
