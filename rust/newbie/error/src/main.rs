use std::fs::File;
use std::fs;
use std::io::ErrorKind;
use std::io::Read;
use std::io;


struct Guess {
    value : i32
}

impl Guess {
    pub fn new(value: i32) -> Guess {
        if value < 1 || value > 100 {
            panic!("Guess should be between 1 and 100");
        }

        Guess {
            value
        }
    }
    pub fn value(&self) -> i32 {
        self.value
    }
}

fn main() {

//    test_panic();

    let fname = "hello.txt";
//    check_file(&fname);
//    check_file2(&fname);
//    check_file3(&fname);
//    check_file4(&fname);
//    let res = read_user_from_file(fname);
//    let res = read_user_from_file2(fname);
//    let res = read_user_from_file3(fname);
    let res = read_user_from_file4(fname);
    match res {
        Ok(users) => println!("Users from file: {}", users),
        Err(e) => panic!("Error read file: {:#?}", e),
    }
}

fn test_panic() {
    //panic!("crash and burn");
    let g = Guess::new(120);
   
    let v = vec![1,2,3];
    v[32];
}

fn check_file(fname : &str) {
    let f = File::open(fname);
    let f = match f {
        Ok(file) => file,
        Err(err) => match err.kind() {
            ErrorKind::NotFound => {
                println!("File not exists. Create it.");

                match File::create(fname) {
                    Ok(fc) => fc,
                    Err(e) => panic!("Try to create file but failed: {:#?}", e),
                }
            },
            other_error =>
                panic!("Try open file but failed: {:#?}", err),
        }
    };

}


// use unwrap_or_else to simplify match cases
fn check_file2(fname : &str) {
    let f = File::open(fname).unwrap_or_else(|error| {
        if error.kind() == ErrorKind::NotFound {
            println!("File not exists. Create it.");
            File::create(fname).unwrap_or_else(|error| {
                panic!("Try to create file but failed: {:?}", error);
            })
        } else {
            panic!("Try to open file but failed: {:#?}", error);
        }
    });
}

// unwrap will call panic for us
fn check_file3(fname : &str) {
    let f = File::open(fname).unwrap(); 
}

// expect will panic with prompt
fn check_file4(fname : &str) {
    let f = File::open(fname).expect("Failed to open file.");
}

fn read_user_from_file(fname : &str) -> Result<String, io::Error> {
    let f = File::open(fname);    

    let mut f = match f {
        Ok(file) => file,
        Err(e) => return Err(e),
    };

    let mut s = String::new();
    match f.read_to_string(&mut s) {
        Ok(_) => Ok(s),
        Err(e) => Err(e),
    }
}

// use '?' to simplify match cases
fn read_user_from_file2(fname :&str) -> Result<String, io::Error> {
    let mut f = File::open(fname)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    Ok(s)
}

// more simplication using '?'
fn read_user_from_file3(fname : &str) -> Result<String, io::Error> {
    let mut s = String::new();
    File::open(fname)?.read_to_string(&mut s)?;
    Ok(s)
}

// rust把上面的功能都集中 read_to_string 函数里了
fn read_user_from_file4(fname : &str) -> Result<String, io::Error> {
    fs::read_to_string(fname)
}

