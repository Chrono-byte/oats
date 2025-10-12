# Memory Design

Concise memory model and ownership guidance for contributors.

## Object Layout
- Header (u64) at offset 0: strong refcount + flags
- Meta-slot at offset 8: pointer to field metadata
- Fields/data at offset 16+

## Reference Types
- Strong: default, increments RC
- Weak: optional, use `rc_weak_inc` / `rc_weak_upgrade`
- Unowned: non-owning raw pointers (proposal, use cautiously)

## Cycle Collector
- Trial-deletion algorithm is the planned safety net for cycles. See `docs/archive/CCRC.md` for full details.

## Hardening
- Use checked arithmetic for allocation sizes
- Respect resource limits (heap and source size)
