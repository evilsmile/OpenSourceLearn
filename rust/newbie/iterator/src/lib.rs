#[derive(PartialEq, Debug)]
struct Shoe {
    size: u32,
    style: String,
}

// shoes接管了所有权
fn shoes_in_my_size(shoes: Vec<Shoe>, shoe_size: u32) -> Vec<Shoe> {
    // into_iter 这个迭代器会接管所有权
    shoes.into_iter()
        .filter(|s| s.size == shoe_size)
        .collect()
}



// 自定义一个Iterator
#[derive(Debug)]
struct Counter {
    count: u32,
}

impl Counter {
    fn new() -> Counter {
        Counter{ count: 0}
    }
}

impl Iterator for Counter {
    type Item = u32;
    fn next(&mut self) -> Option<Self::Item> {
        self.count += 1;
        if self.count < 6 {
            Some(self.count)
        }  else {
            None
        }
    }
}



#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn filters_by_size() {
        let shoes = vec![
            Shoe{size: 10, style: String::from("sneaker")},
            Shoe{size: 13, style: String::from("sandal")},
            Shoe{size: 10, style: String::from("boot")},
        ];

        let in_my_size = shoes_in_my_size(shoes, 10);

        assert_eq!(
            in_my_size,
            vec![
            Shoe{size: 10, style: String::from("sneaker")},
            Shoe{size: 10, style: String::from("boot")},
            ]
            );
    }

    #[test]
    fn call_my_iter() {
        let mut counter =  Counter::new();
        assert_eq!(counter.next(), Some(1));
        assert_eq!(counter.next(), Some(2));
        assert_eq!(counter.next(), Some(3));
        assert_eq!(counter.next(), Some(4));
        assert_eq!(counter.next(), Some(5));
        assert_eq!(counter.next(), None);
    }

    #[test]
    fn using_other_iterator_trait_methods() {
        // 自定义实现的Counter符合Iterator特性，也就可以调用其方法
        let sum: u32 = Counter::new().zip(Counter::new().skip(1))
            .map(|(a,b)| a*b)
            .filter(|x| x % 3 == 0)
            .sum();
        assert_eq!(18, sum);
    }
}
