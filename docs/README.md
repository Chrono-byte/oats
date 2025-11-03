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

**Missing features:** Advanced generics (constraints), decorators, mapped types, conditional types, full class modifiers.

### Core Documentation

#### [`ARCHITECTURE.md`](ARCHITECTURE.md) ⭐ START HERE for technical understanding

#### [`ROADMAP.md`](ROADMAP.md) ⭐ START HERE for project direction
