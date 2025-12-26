/**
 * A simple memory test for Oats runtime
 */

export function main(): void {
    console.log("Starting simple memory test...");

    // Allocate some numbers
    const arr: number[] = [];

    console.log("Phase 1: Allocating 10 numbers...");
    for (let i: number = 0; i < 10; i = i + 1) {
        arr[i] = i;
    }

    console.log("Allocation complete.");

    // Sum of array
    console.log("Phase 2: Calculating sum...");
    let sum: number = 0;
    for (let j: number = 0; j < 10; j = j + 1) {
        sum = sum + arr[j];
    }

    console.log("Sum: ");
    console.log(sum);
    console.log("Test complete.");
}
