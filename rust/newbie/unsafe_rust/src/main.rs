unsafe fn dangerous() {
}

// call 'C' function
extern "C" {
    fn abs(input: i32) -> i32;
}

static mut COUNTER: u32 = 0;

fn add_to_count(inc: u32) {
    // 修改static mut 需要在unsafe中
    unsafe {
        COUNTER += inc;
    }
}

fn main() {
    let mut num = 5;

    // use 'as' to cast an immutable reference into their raw pointer types.
    let r1 = &num as *const i32;
    let r2 = &mut num as *mut i32;

    unsafe {
        println!("r1 is: {}", *r1);
        println!("r2 is: {}", *r2);
        dangerous();
    }

    let mut v = vec![1,2,3,4,5,6];
    let r = &mut v[..];
    let (a, _b) = r.split_at_mut(3);
    println!("a: {:?}", a);

    // Call 'C' within 'unsafe' block
    unsafe {
        println!("Absolute value of -3 according to C: {}", abs(-3));
    }

    add_to_count(3);
    unsafe {
        println!("COUNTER: {}", COUNTER);
    }
}
