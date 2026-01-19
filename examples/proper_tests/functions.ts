function greet(name: string): string {
    return `Hello, ${name}!`;
}

let multiply = (a: number, b: number): number => a * b;

export function main(): void {
    console.log(greet("World"));
    console.log(`5 * 7 = ${multiply(5, 7)}`);

    return;
}
