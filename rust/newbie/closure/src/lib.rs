use std::thread;
use std::time::Duration;

// Cache保存计算值，如果为None的话调用T计算，否则直接使用
pub struct Cache<T>
where T: Fn(u32) -> u32 
{
    calculation: T,
    value: Option<u32>,
}

impl<T> Cache<T> 
    where T: Fn(u32) -> u32
{
    fn new(calculation: T) -> Cache<T> {
        Cache {
            calculation,
            value: None,
        }
    }

    fn value(&mut self, arg: u32) -> u32 {
        match self.value {
            Some(v) => v,
            None => {
                let v = (self.calculation)(arg);
                self.value = Some(v);
                v
            },
        }
    }
}

pub fn generate_workout(intensity: u32, random_num: u32) {
    let mut expensive_result = Cache::new(|num| {
        println!("calculating slowly...");
        thread::sleep(Duration::from_secs(2));
        num
    });

    if intensity < 25 {
        println!(
            "Today, do {} pushups!",
            expensive_result.value(intensity)
            );
        println!(
            "Next, do {} situps!",
            expensive_result.value(intensity)
            );
    } else {
        if random_num == 3 {
            println!("Take a break today!");
        } else {
            println!(
                "Today, run for {} minutes!",
                expensive_result.value(intensity)
                );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn call_with_different_values() {
        let mut c = Cache::new(|a| a);

        let v1 = c.value(1);
        let v2 = c.value(2);

        assert_eq!(v2, 2);
    }
}
