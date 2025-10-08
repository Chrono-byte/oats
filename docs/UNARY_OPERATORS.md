# Unary Operators Implementation Summary

## Overview
Successfully implemented all unary operators for the Oats TypeScript-to-LLVM compiler. This includes arithmetic, logical, bitwise, and update operators.

## Implemented Operators

### 1. Arithmetic Unary Operators
- **Unary Minus (`-`)**: Negates numeric values
  - LLVM: `fneg` instruction for floating-point negation
  - Example: `-x` where x=5 yields -5

- **Unary Plus (`+`)**: Type coercion to number (no-op for numbers)
  - Implemented as identity operation (returns value as-is)
  - Example: `+x` where x=5 yields 5

### 2. Logical Operators
- **Logical NOT (`!`)**: Boolean negation
  - LLVM: `xor i1 %val, true` instruction
  - Converts operand to boolean, then inverts
  - Example: `!true` yields false

### 3. Bitwise Operators
- **Bitwise NOT (`~`)**: Two's complement bitwise inversion
  - LLVM: Convert to i32, apply `xor`, convert back to f64
  - Operations: `fptosi` → `xor` → `sitofp`
  - Example: `~5` yields -6 (two's complement)

### 4. Update Operators
- **Postfix Increment (`x++`)**: Returns old value, increments variable
  - LLVM: Load → Add 1.0 → Store → Return old value
  - Example: `x++` where x=5 returns 5, sets x to 6

- **Postfix Decrement (`x--`)**: Returns old value, decrements variable
  - LLVM: Load → Subtract 1.0 → Store → Return old value
  - Example: `x--` where x=5 returns 5, sets x to 4

- **Prefix Increment (`++x`)**: Increments variable, returns new value
  - LLVM: Load → Add 1.0 → Store → Return new value
  - Example: `++x` where x=5 returns 6, sets x to 6

- **Prefix Decrement (`--x`)**: Decrements variable, returns new value
  - LLVM: Load → Subtract 1.0 → Store → Return new value
  - Example: `--x` where x=5 returns 4, sets x to 4

## Implementation Details

### AST Structure (deno_ast)
```rust
// Unary operators: -, +, !, ~
ast::Expr::Unary(UnaryExpr {
    op: UnaryOp,  // Minus, Plus, Bang, Tilde
    arg: Box<Expr>,
})

// Update operators: ++, --
ast::Expr::Update(UpdateExpr {
    op: UpdateOp,  // PlusPlus, MinusMinus
    prefix: bool,  // true for ++x, false for x++
    arg: Box<Expr>,
})
```

### Code Location
- **Main implementation**: `crates/oats/src/codegen/expr.rs`
  - Lines 1571-1699: Unary operator handling
  - Lines 1700-1764: Update operator handling

### Key Features
1. **Type Safety**: Operators validate operand types
2. **Error Handling**: Clear diagnostics for invalid operations
3. **Mutability Checks**: Update operators verify variable is not const
4. **LLVM Optimization**: Constant folding handled by LLVM

### Limitations
- Update operators only work on simple identifiers (not `obj.field++` or `arr[i]++`)
- Cannot update function parameters directly (immutable)
- Bitwise NOT operates on 32-bit integers (standard TypeScript behavior)

## Testing

### Test Suite
- **Location**: `crates/oats/tests/unary_operators.rs`
- **Tests**: 5 integration tests
  1. `test_unary_minus`: Validates fneg instruction
  2. `test_logical_not`: Validates xor instruction
  3. `test_bitwise_not`: Validates fptosi/xor/sitofp chain
  4. `test_postfix_increment`: Validates fadd instruction
  5. `test_prefix_decrement`: Validates fsub instruction

### Example Programs
- `examples/unary_minus.oats`: Simple negation test
- `examples/unary_operators.oats`: All operators with expected output
- `examples/comprehensive_unary.oats`: Real-world scenarios (73 lines)

### Test Results
- **Total tests**: 51 passing (up from 44 before implementation)
- **New tests**: 5 unary operator tests
- **All existing tests**: Still passing (no regressions)

## Usage Examples

```typescript
// Arithmetic
let x: number = 5;
let neg: number = -x;     // -5
let pos: number = +x;     // 5

// Logical
let flag: boolean = !true;  // false

// Bitwise
let bits: number = ~5;     // -6

// Update operators
let count: number = 0;
count++;                   // count = 1
let old: number = count++; // old = 1, count = 2
let new: number = ++count; // new = 3, count = 3
```

## Roadmap Status
✅ **Short-term items completed**: 5 out of 6
- ✅ While loops
- ✅ Do-while loops
- ✅ Break statement
- ✅ Continue statement
- ✅ Unary operators
- ❌ Switch statements (remaining)

## Next Steps
The last remaining short-term feature is **switch statements**. After that, the compiler will move to medium-term features:
- Arrow functions with closure capture
- Object literals
- Module imports/exports
- Interface types
