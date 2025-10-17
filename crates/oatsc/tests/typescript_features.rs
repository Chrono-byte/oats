//! Test that all high-priority TypeScript features parse correctly
//!
//! This test validates that the Oats parser correctly handles all the critical
//! TypeScript language features mentioned in the project roadmap:
//! - async/await syntax
//! - Classes with all modifiers
//! - Destructuring (arrays and objects)
//! - Template literals
//! - And other ES6+ features

use oatsc::parser::parse_oats_module;

#[test]
fn test_async_await() {
    let code = r#"
        async function fetchData() {
            const response = await fetch('https://api.example.com/data');
            const data = await response.json();
            return data;
        }

        async function* asyncGenerator() {
            yield await Promise.resolve(1);
            yield await Promise.resolve(2);
        }

        const asyncArrow = async () => {
            return await Promise.resolve(42);
        };

        class AsyncClass {
            async method() {
                return await this.helper();
            }
            async helper() {
                return 123;
            }
        }
    "#;
    
    parse_oats_module(code, None).expect("async/await should parse");
}

#[test]
fn test_classes() {
    let code = r#"
        class BaseClass {
            constructor(public name: string) {}
            
            public publicMethod() {}
            protected protectedMethod() {}
            private privateMethod() {}
            
            static staticMethod() {}
            abstract abstractMethod(): void;
        }

        abstract class AbstractClass {
            abstract requiredMethod(): void;
            concreteMethod() {
                console.log('concrete');
            }
        }

        class DerivedClass extends BaseClass {
            #privateField = 42;
            
            constructor(name: string) {
                super(name);
            }
            
            get accessor() {
                return this.#privateField;
            }
            
            set accessor(value: number) {
                this.#privateField = value;
            }
            
            abstractMethod() {
                console.log('implemented');
            }
        }

        class WithPrivateFields {
            #privateField: number;
            #privateMethod() {
                return this.#privateField;
            }
        }
    "#;
    
    parse_oats_module(code, None).expect("classes should parse");
}

#[test]
fn test_destructuring() {
    let code = r#"
        // Array destructuring
        const [a, b, c] = [1, 2, 3];
        const [first, ...rest] = [1, 2, 3, 4, 5];
        const [x, , y] = [1, 2, 3];

        // Object destructuring
        const {name, age} = {name: 'Alice', age: 30};
        const {prop: renamed} = {prop: 'value'};
        const {nested: {deep}} = {nested: {deep: 'value'}};
        const {withDefault = 42} = {};

        // Function parameters
        function processArray([first, second]: number[]) {
            return first + second;
        }

        function processObject({x, y}: {x: number, y: number}) {
            return x * y;
        }

        // Nested destructuring
        const {a: [b, c], d} = {a: [1, 2], d: 3};
    "#;
    
    parse_oats_module(code, None).expect("destructuring should parse");
}

#[test]
fn test_template_literals() {
    let code = r#"
        const name = 'World';
        const greeting = `Hello, ${name}!`;
        
        const multiline = `
            This is a
            multiline
            template literal
        `;
        
        const nested = `outer ${`inner ${42}`} end`;
        
        const withExpression = `The answer is ${40 + 2}`;
        
        function tag(strings: TemplateStringsArray, ...values: any[]) {
            return strings[0] + values[0] + strings[1];
        }
        
        const tagged = tag`value: ${42}`;
    "#;
    
    parse_oats_module(code, None).expect("template literals should parse");
}

#[test]
fn test_arrow_functions() {
    let code = r#"
        const simple = () => 42;
        const withParam = (x: number) => x * 2;
        const withBody = (x: number) => {
            return x * 2;
        };
        const multiParam = (a: number, b: number) => a + b;
        const withRestParam = (...args: number[]) => args.reduce((a, b) => a + b, 0);
        const asyncArrow = async () => await Promise.resolve(42);
    "#;
    
    parse_oats_module(code, None).expect("arrow functions should parse");
}

#[test]
fn test_for_await_of() {
    let code = r#"
        async function processAsyncIterable() {
            const asyncIterable = {
                async *[Symbol.asyncIterator]() {
                    yield 1;
                    yield 2;
                    yield 3;
                }
            };
            
            for await (const value of asyncIterable) {
                console.log(value);
            }
        }
    "#;
    
    parse_oats_module(code, None).expect("for await...of should parse");
}

#[test]
fn test_spread_operator() {
    let code = r#"
        // Array spread
        const arr1 = [1, 2, 3];
        const arr2 = [...arr1, 4, 5];
        const combined = [...arr1, ...arr2];

        // Object spread
        const obj1 = {a: 1, b: 2};
        const obj2 = {...obj1, c: 3};
        const merged = {...obj1, ...obj2};

        // Function arguments
        function sum(...args: number[]) {
            return args.reduce((a, b) => a + b, 0);
        }
        const result = sum(...arr1);
    "#;
    
    parse_oats_module(code, None).expect("spread operator should parse");
}

#[test]
fn test_decorators() {
    let code = r#"
        function sealed(target: any) {
            return target;
        }

        function property(target: any, key: string) {
            console.log(`Property: ${key}`);
        }

        @sealed
        class DecoratedClass {
            @property
            name: string;

            @property
            method() {}
        }
    "#;
    
    parse_oats_module(code, None).expect("decorators should parse");
}

#[test]
fn test_computed_property_names() {
    let code = r#"
        const propName = 'dynamicProp';
        
        const obj = {
            [propName]: 'value',
            ['computed' + 'Key']: 42,
            [Symbol.iterator]: function*() {
                yield 1;
            }
        };

        class WithComputedProperties {
            [propName]: string;
            ['method' + 'Name']() {}
        }
    "#;
    
    parse_oats_module(code, None).expect("computed property names should parse");
}

#[test]
fn test_generators() {
    let code = r#"
        function* simpleGenerator() {
            yield 1;
            yield 2;
            yield 3;
        }

        function* generatorWithReturn() {
            yield 1;
            return 2;
        }

        function* delegatingGenerator() {
            yield* simpleGenerator();
            yield 4;
        }

        class WithGenerator {
            *method() {
                yield 1;
            }
        }
    "#;
    
    parse_oats_module(code, None).expect("generators should parse");
}

#[test]
fn test_optional_chaining_and_nullish_coalescing() {
    let code = r#"
        const obj: any = null;
        const value1 = obj?.property;
        const value2 = obj?.method();
        const value3 = obj?.[0];
        const value4 = obj ?? 'default';
        const value5 = obj?.nested?.deep?.property ?? 'fallback';
    "#;
    
    parse_oats_module(code, None).expect("optional chaining and nullish coalescing should parse");
}

#[test]
fn test_using_declarations() {
    let code = r#"
        {
            using resource = getResource();
            // resource will be disposed at end of scope
        }

        {
            await using asyncResource = getAsyncResource();
            // asyncResource will be async disposed
        }
    "#;
    
    parse_oats_module(code, None).expect("using declarations should parse");
}

#[test]
fn test_numeric_separators() {
    let code = r#"
        const billion = 1_000_000_000;
        const hex = 0xFF_FF_FF_FF;
        const binary = 0b1010_0001_1000_0101;
        const octal = 0o755_000;
    "#;
    
    parse_oats_module(code, None).expect("numeric separators should parse");
}

#[test]
fn test_rest_and_default_parameters() {
    let code = r#"
        function withDefaults(a: number = 1, b: string = 'default') {
            return `${a}: ${b}`;
        }

        function withRest(first: number, ...rest: number[]) {
            return first + rest.reduce((a, b) => a + b, 0);
        }

        function combined(a: number = 0, ...rest: number[]) {
            return a + rest.length;
        }
    "#;
    
    parse_oats_module(code, None).expect("rest and default parameters should parse");
}

#[test]
fn test_complex_types() {
    let code = r#"
        type StringOrNumber = string | number;
        type Point = {x: number, y: number};
        type ReadonlyPoint = Readonly<Point>;
        type PartialPoint = Partial<Point>;
        type RequiredPoint = Required<PartialPoint>;

        interface IBase {
            name: string;
        }

        interface IDerived extends IBase {
            age: number;
        }

        type Conditional<T> = T extends string ? string[] : number[];
        type Mapped<T> = {[K in keyof T]: T[K][]};

        function generic<T extends IBase>(value: T): T {
            return value;
        }
    "#;
    
    parse_oats_module(code, None).expect("complex types should parse");
}

#[test]
fn test_namespaces_and_modules() {
    let code = r#"
        namespace MyNamespace {
            export class MyClass {}
            export function myFunction() {}
            export const myConst = 42;
        }

        namespace Nested.Namespace {
            export interface MyInterface {}
        }

        const instance = new MyNamespace.MyClass();
    "#;
    
    parse_oats_module(code, None).expect("namespaces should parse");
}

#[test]
fn test_enums() {
    let code = r#"
        enum Color {
            Red,
            Green,
            Blue
        }

        enum Direction {
            Up = "UP",
            Down = "DOWN",
            Left = "LEFT",
            Right = "RIGHT"
        }

        enum Mixed {
            No = 0,
            Yes = "YES"
        }

        const color: Color = Color.Red;
    "#;
    
    parse_oats_module(code, None).expect("enums should parse");
}
