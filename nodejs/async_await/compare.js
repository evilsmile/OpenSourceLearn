function takeLongTime(n) {
    return new Promise(resolve => {
        setTimeout(() => resolve(n+200), n);
    });
}
function step1(n) {
    console.log(`step1 with ${n}`);
    return takeLongTime(n);
}
function step2(m, n) {
    console.log(`step2 with ${m} and ${n}`);
    return takeLongTime(n + m);
}
function step3(k, m, n) {
    console.log(`step3 with ${k}, ${m}, and ${n}`);
    return takeLongTime(k + m + n);
}

function doIt() {
    console.time("doIt");
    const time1 = 300;
    step1(time1)
    .then(time2 => {
        return step2(time1, time2)
        .then(time3 => [time1, time2, time3]);
    })
    .then(times => {
        const [time1, time2, time3] = times;
        return step3(time1, time2, time3);
    })
    .then(result=> {
        console.log(`result is ${result}`);
        console.timeEnd("doIt");
    });
}

doIt()
// step1 with 300
// step2 with 500
// step3 with 700
// result is 900
// doIt: 1505.625ms



// 使用 async/await 实现和前面的Promise实现一样,但这个代码看起来更清晰
// 几乎和同步代码一样
async function doIt2() {
    console.time("doIt2");
    const time1 = 300;
    const time2 = await step1(time1);
    const time3 = await step2(time1, time2);
    const result = await step3(time1, time2, time3);
    console.log(`result is ${result}`);
    console.timeEnd("doIt2");
}
doIt2();
