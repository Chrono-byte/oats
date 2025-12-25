//! Class generation for code generation.
//!
//! This module contains code generation logic for class declarations,
//! including methods and registering field metadata.

use crate::diagnostics::Diagnostic;
use crate::types::SymbolTable;
use oxc_ast::ast::{
    BindingPatternKind, Class, ClassElement, Expression, MethodDefinitionKind, PropertyKey,
};

impl<'a> crate::codegen::CodeGen<'a> {
    /// Generates IR for a class declaration.
    pub fn gen_class_ir(
        &self,
        c: &'a Class<'a>,
        prefix: &str,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        let (class_name, raw_name) = if let Some(id) = &c.id {
            let rname = id.name.to_string();
            let mangled = if prefix.is_empty() {
                rname.clone()
            } else {
                format!("{prefix}_{rname}")
            };
            (mangled, rname)
        } else {
            return Ok(());
        };

        // Check if this is a generic class
        let type_params = if let Some(tp) = &c.type_parameters {
            tp.params
                .iter()
                .map(|p| p.name.name.to_string())
                .collect::<Vec<_>>()
        } else {
            Vec::new()
        };

        // If generic, create a substitution that replaces type parameters with unions
        let subst = if type_params.is_empty() {
            None
        } else {
            let mut s = std::collections::HashMap::new();
            for tp in &type_params {
                // Replace all type parameters with a union of number and string
                s.insert(
                    tp.clone(),
                    crate::types::OatsType::Union(vec![
                        crate::types::OatsType::Number,
                        crate::types::OatsType::String,
                    ]),
                );
            }
            Some(s)
        };

        // Store generic classes for potential future use
        if !type_params.is_empty() {
            self.nested_generic_classes
                .borrow_mut()
                .insert(raw_name.clone(), (c, type_params));
        }

        // Register in symbol table
        self.symbol_table
            .borrow_mut()
            .insert_type(raw_name, class_name.clone());

        // 1. Record parent class
        let parent_name_opt = if let Some(super_class) = &c.super_class
            && let Expression::Identifier(id) = super_class
        {
            let sname = id.name.to_string();
            // Resolve parent name through symbol table
            let symbols = self.symbol_table.borrow();
            if let Some(crate::types::Symbol::Type { mangled_name }) = symbols.get(&sname) {
                Some(mangled_name.clone())
            } else {
                Some(sname)
            }
        } else {
            None
        };
        self.class_parents
            .borrow_mut()
            .insert(class_name.clone(), parent_name_opt.clone());

        // 2. Scan members for fields and constructor FIRST
        let mut fields = Vec::new();
        let mut ctor_opt = None;

        for member in &c.body.body {
            match member {
                ClassElement::MethodDefinition(m) => {
                    if m.kind == MethodDefinitionKind::Constructor {
                        ctor_opt = Some(m);
                    }
                }
                ClassElement::PropertyDefinition(p) => {
                    // Extract field name and type
                    let pname = match &p.key {
                        PropertyKey::StaticIdentifier(id) => id.name.to_string(),
                        PropertyKey::Identifier(id) => id.name.to_string(),
                        PropertyKey::StringLiteral(s) => s.value.to_string(),
                        _ => continue,
                    };
                    let ptype = if let Some(ann) = &p.type_annotation {
                        if let Some(ref s) = subst {
                            crate::types::map_ts_type_with_subst_and_aliases(
                                &ann.type_annotation,
                                s,
                                Some(&*self.type_aliases.borrow()),
                                Some(&*self.symbol_table.borrow()),
                            )
                        } else {
                            crate::types::map_ts_type_with_aliases(
                                &ann.type_annotation,
                                Some(&*self.type_aliases.borrow()),
                                Some(&*self.symbol_table.borrow()),
                            )
                        }
                        .unwrap_or(crate::types::OatsType::Number)
                    } else {
                        crate::types::OatsType::Number
                    };
                    fields.push((pname, ptype));
                }
                _ => {}
            }
        }

        // Add shorthand properties from constructor if found
        if let Some(ctor) = ctor_opt {
            for param in &ctor.value.params.items {
                if let BindingPatternKind::BindingIdentifier(ident) = &param.pattern.kind {
                    let pname = ident.name.to_string();
                    if !fields.iter().any(|(n, _)| n == &pname) {
                        let ptype = if let Some(ann) = &param.pattern.type_annotation {
                            if let Some(ref s) = subst {
                                crate::types::map_ts_type_with_subst_and_aliases(
                                    &ann.type_annotation,
                                    s,
                                    Some(&*self.type_aliases.borrow()),
                                    Some(&*self.symbol_table.borrow()),
                                )
                            } else {
                                crate::types::map_ts_type_with_aliases(
                                    &ann.type_annotation,
                                    Some(&*self.type_aliases.borrow()),
                                    Some(&*self.symbol_table.borrow()),
                                )
                            }
                            .unwrap_or(crate::types::OatsType::Number)
                        } else {
                            crate::types::OatsType::Number
                        };
                        fields.push((pname, ptype));
                    }
                }
            }
        }

        // Register fields BEFORE generating methods
        self.class_fields
            .borrow_mut()
            .insert(class_name.clone(), fields.clone());

        // 3. Generate methods and constructor
        for member in &c.body.body {
            if let ClassElement::MethodDefinition(m) = member
                && m.kind != MethodDefinitionKind::Constructor
            {
                let mname = match &m.key {
                    PropertyKey::StaticIdentifier(id) => id.name.to_string(),
                    PropertyKey::Identifier(id) => id.name.to_string(),
                    PropertyKey::StringLiteral(s) => s.value.to_string(),
                    _ => continue,
                };

                // Skip dead methods if RTA results are available
                if let Some(ref rta) = self.rta_results
                    && !rta.is_method_live(&class_name, &mname)
                {
                    continue;
                }

                let method_func = &m.value;
                let mut method_symbols = SymbolTable::new();
                let (sig_opt, _) =
                    crate::types::check_function_strictness(method_func, &mut method_symbols)
                        .map_err(|e| Box::new(Diagnostic::error(e.to_string())))?;

                if let Some(sig) = sig_opt {
                    let mut params = Vec::new();
                    params.push(crate::types::OatsType::NominalStruct(class_name.clone()));
                    params.extend(sig.params.into_iter());
                    let ret = sig.ret;
                    let fname = format!("{class_name}_{mname}");

                    // Save current class parent for super calls
                    let old_parent = self.current_class_parent.borrow().clone();
                    *self.current_class_parent.borrow_mut() = parent_name_opt.clone();

                    self.gen_function_ir(&fname, method_func, &params, &ret, Some("this"))?;

                    *self.current_class_parent.borrow_mut() = old_parent;
                }
            }
        }

        if let Some(ctor) = ctor_opt {
            // Save current class parent for constructor
            let old_parent = self.current_class_parent.borrow().clone();
            *self.current_class_parent.borrow_mut() = parent_name_opt;

            self.gen_constructor_ir(&class_name, ctor, &fields, None)?;

            *self.current_class_parent.borrow_mut() = old_parent;
        } else {
            // Generate default constructor for classes without explicit constructors
            // This allows `new ClassName()` to work even when no constructor is defined
            self.gen_default_constructor_ir(&class_name, &fields)?;
        }

        Ok(())
    }
}
