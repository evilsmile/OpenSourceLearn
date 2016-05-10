namespace cpp lj.test
struct student {
	1: i32 sno,
	2: string name,		 
	3: bool ssex,
	4: i16 sage
}

service Serv {
	i32 put(1: student s),
}
