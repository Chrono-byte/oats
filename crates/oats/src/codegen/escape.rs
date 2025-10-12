use deno_ast::swc::ast;

/// EscapeInfo: conservative summary of which local names escape the current
/// function. For now it's just a set of local names that must be treated as
/// escaping by the emitter.
#[derive(Clone, Debug, Default)]
pub struct EscapeInfo {
    pub escaping_locals: std::collections::HashSet<String>,
}

/// Analyze a function AST and return a conservative EscapeInfo. Current
/// implementation is a no-op (everything is treated as escaping). This will be
/// refined in follow-up commits.
pub fn analyze_fn(_func: &ast::Function) -> EscapeInfo {
    // TODO: implement intra-procedural def-use/escape analysis.
    EscapeInfo::default()
}
