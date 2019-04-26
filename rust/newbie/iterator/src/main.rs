fn main() {

    let v1: Vec<i32> = vec![1,2,3];

    // map是 iterator的方法，以闭包为参数，产生一个新的iterator
    // 注意： iterator adaptors are lazy，所以这里加了collect()，否则什么也没有执行
    let v2: Vec<_> = v1.iter().map(|x| x+1).collect();

    assert_eq!(v2, vec![2,3,4]);
}
