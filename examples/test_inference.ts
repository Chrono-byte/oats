export function main(): number {
    let x: number | string = 5;
    let y = 0.1;
    let s = "hello";
    let arr = [1, 2, 3];

    console.log(x);
    console.log(y);
    console.log(s);
    console.log(arr[0]);

    x = "hello";
    console.log(x);

    return 0;
}

