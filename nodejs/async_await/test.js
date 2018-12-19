// async输出一个Promise对象
async function testAsync() {
    // 在async中return一个直接量,async会把这个直接量通过Promise.resolve()封装成Promise对象
    return "Hello async";
}

const result = testAsync();
console.log(result);   // Promise {"Hello async"}
