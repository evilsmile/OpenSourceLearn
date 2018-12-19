async function testAsync() {
    return "hello async"
}

testAsync().then( v => {
    console.log(v)
});
