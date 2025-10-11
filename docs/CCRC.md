# Cycle-Collecting Reference Counting

**Status:** Design Complete, Implementation Planned for Phase 4  
**Last Updated:** October 10, 2025

## Purpose

Provide a compact, actionable design that eliminates leaks caused by reference cycles while keeping deterministic ARC semantics for the common case.

Two complementary features:

- Weak references (manual): enable non-owning references that do not keep the object alive.
- Automatic cycle collector (automatic): detect and reclaim cyclic garbage when weak references aren't used.

Key invariants
--------------
- Compiler must emit a `field_map` (per-class metadata) and store a pointer to it in the object's meta slot (offset +8).
- The runtime expects header+meta+fields layout: header@0, meta@8, fields@16+.

Weak references (implementation sketch)
--------------------------------------
- Add weak-count into header (bits 33..48).
- Runtime APIs: rc_weak_inc(ptr), rc_weak_dec(ptr), rc_weak_upgrade(ptr) [returns NULL if object already destroyed].
- Compiler: add OatsType::Weak(T) and Option<T> support; emit rc_weak_inc/rc_weak_dec where appropriate.
- Semantics: downgrade() produces Weak<T>, upgrade() attempts atomic promotion (returns null/None if object gone).

Automatic cycle collector (trial-deletion summary)
-------------------------------------------------
1. Root list: when an object's strong count is decremented but remains > 0, add it to the root list (candidate for cycle).
2. Collector trigger: run when root list size exceeds threshold.
3. Trial deletion: for each candidate, recursively decrement (temporarily) reachable strong counts using only pointer fields from `field_map`.
4. Sweep: objects with temporary count == 0 are unreachable from outside the cycle — call destructors and reclaim them.
5. Restore: for objects not collected, restore counts to original values.

Notes
-----
- This algorithm requires accurate per-class pointer-field metadata. If codegen doesn't emit correct metadata, the collector is unsafe.
- The collector must be careful to not run concurrently with non-atomic mutations unless appropriate synchronization is present.

Milestones (practical)
----------------------
1. Implement Weak<T> support in runtime + compiler (low risk, high value).
2. Emit field_map metadata alongside constructors and store pointer in meta slot (ensure meta-slot invariant).
3. Implement root-list and a single-threaded trial-deletion collector (debug-build first).
4. Add tests (small cycles, larger graphs) and measure overhead; tune trigger threshold.

When to use each feature
------------------------
- Prefer Weak<T> for explicit cycle-breaking in data structures (trees, graphs).
- Use the automatic collector as a safety net for user code that doesn't use Weak<T>.

This doc is intentionally short — for the full rationale and the implementation narrative see `DEVELOPMENT.md` and `ARCHITECTURE.md`.

---

**Related Documents:**
- `ARCHITECTURE.md` - Memory layout and RC fundamentals
- `ROADMAP.md` - Phase 4 timeline and priorities  
- `DEVELOPMENT.md` - Implementation guidelines