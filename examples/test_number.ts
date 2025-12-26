// Test Number functionality to ensure JavaScript compliance

export function main(): void {
    // Test Number static properties
    const epsilon = Number.EPSILON;
    const maxSafeInt = Number.MAX_SAFE_INTEGER;
    const maxVal = Number.MAX_VALUE;
    const minSafeInt = Number.MIN_SAFE_INTEGER;
    const minVal = Number.MIN_VALUE;
    const nan = Number.NaN;
    const negInf = Number.NEGATIVE_INFINITY;
    const posInf = Number.POSITIVE_INFINITY;

    // Test Number static methods
    const isNan = Number.isNaN(NaN);
    const isFinite = Number.isFinite(42);
    const isInteger = Number.isInteger(42);
    const isSafeInt = Number.isSafeInteger(42);
    const parsedFloat = Number.parseFloat("3.14");
    const parsedInt = Number.parseInt("42", 10);

    // Test number literals and arithmetic
    const num = 42.5;
    const result = num + 1;

    // Test Number instance methods
    const str = num.toString();
    const fixed = num.toFixed(2);
    const exp = num.toExponential(2);
    const prec = num.toPrecision(4);
    const locale = num.toLocaleString();
    const value = num.valueOf();

    // Test special values
    const nanStr = NaN.toString();
    const infStr = Infinity.toString();
    const negInfStr = (-Infinity).toString();

    // Test Number constructor
    const numObj = new Number(42);
    const numVal = numObj.valueOf();

    console.log("Number functionality test completed");
}
