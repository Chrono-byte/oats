# Oats Documentation

Documentation for the Oats compiler.

## Quick Start

- Main `README.md` in the repo root
- `DEVELOPMENT.md` for setup
- `examples/` to see it in action
- `ARCHITECTURE.md` for how it works

## Contributing

- `DEVELOPMENT.md` - How to work on the code
- `ROADMAP.md` - What's planned

## What Works

Oats supports most TypeScript/JavaScript:

- Core features: functions, classes, interfaces, enums, generics, unions
- Async/await with state machine lowering
- Modern syntax: destructuring, template literals, arrow functions
- Types: primitives, tuples, promises, weak/unowned refs
- Memory: ARC with cycle collection, escape analysis
- Packages: multi-file compilation with dependency resolution

Still missing: advanced generics (constraints), decorators, mapped types, conditional types, full class modifiers.

## Core Docs

- [`ARCHITECTURE.md`](ARCHITECTURE.md) - Technical overview
- [`ROADMAP.md`](ROADMAP.md) - What's coming next
