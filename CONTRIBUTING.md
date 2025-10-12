# Contributing to Oats

Thank you for your interest in contributing to Oats! This document provides guidelines and best practices for contributing to the project.

## Table of Contents

1. [Code of Conduct](#code-of-conduct)
2. [Getting Started](#getting-started)
3. [How to Contribute](#how-to-contribute)
4. [Development Workflow](#development-workflow)
5. [Pull Request Process](#pull-request-process)
6. [Coding Standards](#coding-standards)
7. [Testing Requirements](#testing-requirements)
8. [Documentation](#documentation)
9. [Getting Help](#getting-help)

## Code of Conduct

- Be respectful and constructive in all interactions
- Welcome newcomers and help them get started
- Focus on what is best for the project and community
- Show empathy towards other community members

## Getting Started

### Prerequisites

- Rust 1.70 or later
- LLVM 18 with development headers
- Basic understanding of compilers and LLVM IR (helpful but not required)

### Initial Setup

```bash
# Clone the repository
git clone https://github.com/Chrono-byte/oats.git
cd oats

# Setup LLVM environment
source ./scripts/setup_env.sh

# Build the project
cargo build --workspace

# Run tests to verify setup
cargo test --workspace

# Try running an example
cargo run -p oats --bin toasty -- examples/add.oats
./aot_out/add
```

If you encounter issues, see `docs/DEVELOPMENT.md` for troubleshooting tips.

## How to Contribute

### Types of Contributions

**Bug Reports:**
- Search existing issues first to avoid duplicates
- Include minimal reproduction case
- Provide version information and environment details
- Describe expected vs. actual behavior

**Feature Requests:**
- Check the roadmap (`docs/ROADMAP.md`) to see if it's planned
- Open an issue to discuss the feature before implementing
- Explain the use case and benefits
- Consider implementation complexity

**Code Contributions:**
- Start with good first issues (labeled `good-first-issue`)
- Discuss large changes in an issue first
- Follow the coding standards and testing requirements
- Keep pull requests focused and reasonably sized

**Documentation:**
- Fix typos, improve clarity, add examples
- Update docs when implementing features
- Add inline comments for complex logic
- Create tutorials or guides for common tasks

### Good First Issues

Great starting points for new contributors:

1. **Convert `.unwrap()` to Result** - Find a `.unwrap()` call in `crates/oats/src/codegen/` and convert it to proper error handling
2. **Add Runtime Tests** - Write tests in `crates/runtime/tests/` for RC behavior or object layout
3. **Improve Error Messages** - Make diagnostics more helpful with better messages and spans
4. **Add Examples** - Create small, focused example programs demonstrating features
5. **Fix Documentation** - Correct errors or improve clarity in docs/

## Development Workflow

### 1. Create a Branch

```bash
git checkout -b feature/my-feature
# or
git checkout -b fix/bug-description
```

Branch naming conventions:
- `feature/` - New features
- `fix/` - Bug fixes
- `docs/` - Documentation changes
- `refactor/` - Code refactoring
- `test/` - Test additions or fixes

### 2. Make Changes

- Write code following the style guide
- Add tests for new functionality
- Update documentation as needed
- Commit frequently with clear messages

### 3. Test Your Changes

```bash
# Run compiler tests
cargo test -p oats

# Run runtime tests
cargo test -p runtime

# Run all tests
cargo test --workspace

# Check code quality
cargo clippy --workspace -- -D warnings

# Format code
cargo fmt --all

# Review snapshot changes
cargo insta review

# Test examples
./scripts/run_all_proper_tests.sh
```

### 4. Update Documentation

- Update `docs/` if changing architecture or adding features
- Add inline comments for complex logic
- Update examples if API changes
- Add or update tests demonstrating the change

### 5. Commit Your Changes

Write clear, descriptive commit messages:

```
Short summary (50 chars or less)

More detailed explanation if needed. Wrap at 72 characters.
Explain what and why, not how (code shows how).

- Bullet points are okay
- Use present tense: "Add feature" not "Added feature"
- Reference issues: "Fixes #123" or "Related to #456"
```

## Pull Request Process

### Before Submitting

**Pre-submission Checklist:**
- [ ] All tests pass (`cargo test --workspace`)
- [ ] Code is formatted (`cargo fmt --all`)
- [ ] No clippy warnings (`cargo clippy --workspace`)
- [ ] No new `.unwrap()` or `.expect()` in codegen code
- [ ] Documentation updated if needed
- [ ] Snapshot changes reviewed and explained
- [ ] Examples still work

### Submitting a Pull Request

1. **Push your branch**
   ```bash
   git push origin feature/my-feature
   ```

2. **Open a Pull Request** on GitHub

3. **Fill out the PR template:**
   ```markdown
   ## Summary
   Brief description of what this PR does
   
   ## Motivation
   Why is this change needed?
   
   ## Changes
   - List of specific changes made
   - Include any breaking changes
   
   ## Testing
   How was this tested? What tests were added?
   
   ## Screenshots (if applicable)
   For visual changes or error message improvements
   
   ## Checklist
   - [ ] Tests pass locally
   - [ ] Code is formatted and linted
   - [ ] Documentation updated
   - [ ] No new unwrap/expect in codegen
   - [ ] Snapshot changes explained
   
   Fixes #issue_number (if applicable)
   ```

4. **Respond to feedback**
   - Address review comments promptly
   - Make requested changes in new commits
   - Explain your reasoning if you disagree
   - Mark conversations as resolved when addressed

5. **Keep PR updated**
   - Rebase on master if needed: `git pull --rebase origin master`
   - Resolve conflicts carefully
   - Re-run tests after rebasing

### PR Approval Process

- At least one maintainer approval required
- CI must pass (when CI is set up)
- All conversations must be resolved
- No merge conflicts with master
- Snapshot changes must be explained and justified

## Coding Standards

### General Principles

- **Clarity over cleverness** - Write code that's easy to understand
- **No panics in codegen** - All fallible operations return `Result<T, Diagnostic>`
- **Test everything** - New features need tests
- **Document why, not what** - Code shows what, comments explain why

### Error Handling

**✅ DO:**
```rust
pub fn lower_expr(...) -> Result<BasicValueEnum<'a>, Diagnostic> {
    let value = operation()
        .map_err(|_| Diagnostic::simple("Operation failed"))?;
    Ok(value)
}
```

**❌ DON'T:**
```rust
pub fn lower_expr(...) -> BasicValueEnum<'a> {
    let value = operation().unwrap();  // Panic!
    value
}
```

### Memory Management

- **Increment RC when storing** pointers in fields, arrays, or closures
- **Decrement RC before overwriting** or dropping pointers
- **Clean up locals** before `return`, `break`, or `continue`
- **Use `Weak<T>` to break cycles** when appropriate
- **Fields start at offset 16** - Never use offset 8 (reserved for meta-slot)

### Code Style

- Follow Rust standard style (use `cargo fmt`)
- Maximum line length: 100 characters (soft limit)
- Use meaningful variable names
- Add comments for complex logic
- Group related functions together
- Keep functions focused and reasonably sized

## Testing Requirements

### Test Coverage

**All PRs must include tests for:**
- New features
- Bug fixes
- Changed behavior

**Exception:** Documentation-only changes don't need tests.

### Test Types

1. **Unit Tests** (`crates/oats/tests/codegen.rs`)
   - Fast, focused tests
   - Check specific IR patterns
   - Use `gen_ir_for_source()` helper

2. **Snapshot Tests** (`tests/codegen_snapshot.rs`)
   - Capture complete IR output
   - Detect unintended changes
   - Must be reviewed before accepting

3. **Integration Tests** (`tests/end_to_end.rs`)
   - Compile and run actual programs
   - Verify runtime behavior
   - Slower but comprehensive

4. **Runtime Tests** (`crates/runtime/tests/`)
   - Test C ABI functions
   - Validate RC behavior
   - Check memory layout

### Writing Good Tests

```rust
#[test]
fn test_feature_name() {
    let source = r#"
        export function main(): void {
            // Test code here
        }
    "#;
    
    let ir = gen_ir_for_source(source).unwrap();
    
    // Check for expected patterns
    assert!(ir.contains("expected_llvm_instruction"));
    assert!(!ir.contains("should_not_appear"));
}
```

### Snapshot Testing

```bash
# Create or update snapshots
cargo test -p oats

# Review changes
cargo insta review

# Accept all (use carefully!)
INSTA_UPDATE=auto cargo test -p oats
```

**When reviewing snapshots:**
- Understand what changed and why
- Verify the change is intentional
- Explain changes in PR description
- Reject unintended changes

## Documentation

### What to Document

- **Architecture changes** - Update `docs/ARCHITECTURE.md`
- **New features** - Update `docs/ROADMAP.md` and `docs/DEVELOPMENT.md`
- **API changes** - Update function/module documentation
- **Complex algorithms** - Add inline comments explaining the approach
- **Memory layouts** - Update architecture docs if object layouts change

### Documentation Style

- Use clear, concise language
- Include code examples where helpful
- Explain "why" not just "what"
- Keep documentation synchronized with code
- Use Markdown formatting consistently

### Inline Comments

```rust
// ✅ Good - Explains why
// We use i8* instead of struct type to avoid LLVM alignment issues
// with dynamically-sized objects. The runtime knows the actual layout.

// ❌ Bad - States the obvious
// Create a pointer to i8
```
