# Oats Documentation Guide

**Last Updated:** October 18, 2025

Welcome to the Oats compiler documentation! This guide helps you navigate the
documentation and find what you need.

## Quick Start

**New to Oats?** Start here:

1. Read the main `README.md` in the repository root
2. Follow setup instructions in `DEVELOPMENT.md`
3. Try running examples from `examples/`
4. Read `ARCHITECTURE.md` for system overview

**Want to contribute?** Read:

1. `CONTRIBUTING.md` (in repository root) - Contribution guidelines
2. `DEVELOPMENT.md` - Development workflows and standards
3. `ROADMAP.md` - Current priorities and future plans

## Current Implementation Status

**October 18, 2025:** Oats supports a substantial TypeScript subset including:

- ✅ **Core language**: Functions, classes, interfaces, enums, generics, unions
- ✅ **Async/await**: Full `async`/`await` syntax and state machine lowering
- ✅ **Modern syntax**: Destructuring, template literals, arrow functions
- ✅ **Type system**: Primitives, tuples, promises, weak/unowned references
- ✅ **Memory management**: ARC with cycle collection, escape analysis
- ✅ **Package system**: Multi-file compilation with dependency resolution

**Parser coverage:** ~90.43% success rate on TypeScript conformance tests (4,547/5,028 files).

**Missing features:** Advanced generics (constraints), decorators, JSX, mapped types, conditional types, full class modifiers.

## Documentation Structure

### Core Documentation

#### [`ARCHITECTURE.md`](ARCHITECTURE.md) ⭐ START HERE for technical understanding

**Purpose:** Authoritative reference for runtime memory model and compiler
contracts\
**Audience:** All contributors\
**Content:**

**When to read:** Before making changes to object layout, RC behavior, or core
codegen.

#### [`ROADMAP.md`](ROADMAP.md) ⭐ START HERE for project direction

**Purpose:** Project roadmap and current priorities\
**Audience:** Contributors, users, project planners\
**Content:**

- Current implementation status (what's done, what's not)
- Development phases (Stability, Async, Language Features, Cycles)
- Timeline estimates
- Current work items (pick one to implement!)
- Decision log

**When to read:** To understand project priorities and find contribution
opportunities.

### Topics at a Glance

- **Architecture & Language Design:** `ARCHITECTURE.md`
- **Workflow & Testing:** `DEVELOPMENT.md`, `implementation/FUZZING.md`
- **Analysis & Optimization:** `implementation/ESCAPE_ANALYSIS.md`
- **Package System:** `implementation/PACKAGE_SYSTEM.md`
- **FFI Design:** `implementation/ffi.md`
- **Direction:** `ROADMAP.md`, `ROADMAP_SYSTEMS.md`
- **Implementation Details:** `implementation/` directory

## Documentation by Use Case

### "I want to add a new language feature"

1. Read `ARCHITECTURE.md` - Understand object model
2. Read `DEVELOPMENT.md` - "Adding a New Expression Type" or "Adding a New Type"
3. Read `ROADMAP.md` - Check if feature is planned, what phase
4. Write tests first (TDD approach)
5. Implement feature following error handling patterns
6. Update docs if needed

### "I want to fix a bug"

1. Read relevant section of `ARCHITECTURE.md` or `DEVELOPMENT.md`
2. Write a failing test that reproduces the bug
3. Fix the bug
4. Verify test passes and no regressions
5. Update docs if bug revealed incorrect documentation

### "I want to understand async/await"

1. Review the async lowering notes in `ARCHITECTURE.md`
2. Look at `examples/proper_tests/async_await.oats` - Working example
3. Read async sections in `DEVELOPMENT.md` for implementation details

### "I want to improve performance"

1. Read `ARCHITECTURE.md` - Memory layout costs
2. Read `DEVELOPMENT.md` - "Performance Considerations"
3. Profile first (`cargo build --timings`, `perf`, `valgrind`)
4. Identify bottleneck
5. Optimize with tests to prevent regressions

### "I want to add a runtime function"

1. Read `DEVELOPMENT.md` - "Adding a New Runtime Function"
2. Add to `crates/runtime/src/lib.rs` with `#[no_mangle]`
3. Declare and cache it in `crates/oats/src/codegen/mod.rs` (getter + field)
4. Add tests in `crates/runtime/tests/`
5. Document the function purpose and contracts

## Documentation Maintenance

### Keeping Docs Current

**When to update docs:**

- Adding or changing features → Update ROADMAP.md, relevant design docs
- Changing object layout or RC rules → Update ARCHITECTURE.md
- Adding runtime functions or ABIs → Update relevant design docs
- Completing a phase → Update ROADMAP.md status
- Discovering incorrect documentation → Fix immediately

**How to update:**

- Make doc changes in the same PR as code changes
- Explain doc changes in PR description
- Keep docs concise and authoritative
- Add "Last Updated" dates when making significant changes

### Doc Review Checklist

Before merging changes:

- [ ] All affected docs updated
- [ ] No contradictions between docs
- [ ] Examples still work
- [ ] Status headers current
- [ ] Cross-references valid

## Document Status Legend

**Status Tags:**

- ✅ **Current and Accurate** - Reflects current implementation
- ⏳ **Design Document** - Specification for future implementation
- 🚧 **Work in Progress** - Being actively updated
- ⚠️ **Partially Outdated** - Needs review and updating
- 📝 **RFC** - Request for Comments, subject to change

## Getting Help with Documentation

**Documentation issues:**

- **Typos/errors:** Submit a PR with fix
- **Unclear sections:** Open an issue asking for clarification
- **Missing information:** Open an issue or PR adding it
- **Outdated content:** Open an issue flagging what's wrong

**Questions about what to read:**

- Ask in GitHub Discussions
- Check this guide for use cases
- Look at "When to read" sections above

## Contributing to Documentation

**Good documentation contributions:**

- Fixing typos and grammar
- Adding examples and code samples
- Improving clarity and organization
- Adding missing information
- Creating diagrams (Mermaid, ASCII art)
- Writing tutorials and how-to guides

**Documentation style:**

- Use clear, concise language
- Provide examples where helpful
- Use consistent Markdown formatting
- Add code blocks with syntax highlighting
- Use tables for structured information
- Keep line length reasonable (80-100 chars)

See `CONTRIBUTING.md` for full contribution guidelines.

---

**Have questions?** Open an issue or start a discussion on GitHub!

**Found an error?** Submit a PR or open an issue!

**Want to help?** Documentation contributions are always welcome!

## Useful Scripts

- setup_env.sh: configure LLVM environment: `source ./scripts/setup_env.sh`
- run_fuzzing.sh: run fuzzing suite: `./scripts/run_fuzzing.sh`
- run_all_proper_tests.sh: compile all proper_tests examples:
  `./scripts/run_all_proper_tests.sh`
- cloc_no_tests.sh: count code lines excluding tests:
  `./scripts/cloc_no_tests.sh [--extra-args "<args>"]`
