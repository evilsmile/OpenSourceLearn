fn main() {
    let mut x : i32  = 5;
    println!("The value of x is: {}", x);
    x = 6;
    println!("The value of x is: {}", x);
    let x = x + 1;
    println!("The value of x is: {}", x);
    // Compile error. x 被重新定义为不可变的,这里会出错
    // x = 8;
    // println!("The value of x is: {}", x);
    let y = 23_3222;
    let y = 0x234;
    let y = 0b111101_00;
    let y = 0o213;
    let y = b'C';
    let y : char= '我';
    println!("The value of y is: {}", y);

    let tup = (500, 2.2, 3);
    let (x, y, z) = tup;
    println!("The value of z is: {}, {}", z, tup.0);

    let mut arr : [i32; 4] = [1,2,3,4];
    arr[0] = 88;
    println!("The value of arr[0] is: {}", arr[0]);
    // Compile error: 索引超域
    //arr[10] = 23;
    print(arr[3]);
}

fn print(x: i32) -> (i32, bool) {
    println!("The output is: {}", x);

    (3, true)
}
