//! AST to IR lowering
//!
//! This module converts SCALF AST nodes to Sculk IR.

use std::collections::HashMap;

use crate::ir::*;
use crate::CompileError;
use scalf::parser::ast::{BinaryOp as AstBinaryOp, Expr, Program, Stmt, UseTarget};

const HTTP_GET_FN: &str = "__sculk_http_get";
const HTTP_POST_FN: &str = "__sculk_http_post";
const HTTP_PUT_FN: &str = "__sculk_http_put";
const HTTP_DELETE_FN: &str = "__sculk_http_delete";
const HTTP_RESPONSE_STATUS_FN: &str = "__sculk_http_response_status";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ModuleBinding {
    StdHttp,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LoweredValueKind {
    Unknown,
    HttpResponse,
}

#[derive(Debug, Clone)]
struct CallTarget {
    func_name: String,
    result_kind: LoweredValueKind,
}

/// Lower SCALF AST to Sculk IR.
pub struct Lowering {
    /// Next temporary variable ID.
    next_temp: usize,
}

impl Lowering {
    /// Create a new lowering context.
    pub fn new() -> Self {
        Self { next_temp: 0 }
    }

    /// Generate a unique temporary variable name.
    pub fn new_temp(&mut self) -> String {
        let temp = format!("t{}", self.next_temp);
        self.next_temp += 1;
        temp
    }

    /// Lower a parsed SCALF program.
    pub fn lower_program(
        &mut self,
        ast: &Program,
        module_name: &str,
    ) -> Result<Module, CompileError> {
        let mut module = Module::new(module_name.to_string());
        let mut top_level = Vec::new();

        for stmt in &ast.statements {
            match stmt {
                Stmt::FunctionDef {
                    name,
                    params,
                    return_type,
                    body,
                } => {
                    let function =
                        self.lower_function(name, params, return_type.as_deref(), body)?;
                    module.functions.push(function);
                }
                other => top_level.push(other.clone()),
            }
        }

        // Allow script-style programs without explicit `def main`.
        if !top_level.is_empty() {
            let main = self.lower_function("main", &[], None, &top_level)?;
            module.functions.push(main);
        }

        Ok(module)
    }

    fn lower_function(
        &mut self,
        name: &str,
        params: &[scalf::parser::ast::Param],
        declared_return_type: Option<&str>,
        body: &[Stmt],
    ) -> Result<Function, CompileError> {
        let lowered_params = params
            .iter()
            .map(|param| Parameter {
                name: param.name.clone(),
                ty: self.lower_type_annotation(param.type_annotation.as_deref()),
            })
            .collect::<Vec<_>>();

        let return_type = self.infer_return_type(name, declared_return_type, body);
        let mut function = Function::new(name.to_string(), lowered_params, return_type.clone());
        let mut entry = BasicBlock::new("entry".to_string());

        let mut has_terminator = false;
        let mut module_bindings = HashMap::new();
        let mut value_kinds = HashMap::new();
        for param in &function.params {
            value_kinds.insert(param.name.clone(), self.kind_for_ir_type(&param.ty));
        }

        for stmt in body {
            if has_terminator {
                break;
            }

            match stmt {
                Stmt::Use { target, alias } => {
                    self.register_use_import(target, alias.as_deref(), &mut module_bindings)?;
                }
                Stmt::Print { expr } => {
                    let arg = self.lower_expr_value(
                        expr,
                        &mut entry,
                        &module_bindings,
                        &mut value_kinds,
                    )?;
                    entry.instructions.push(Instruction::Call {
                        dest: None,
                        func: "print".to_string(),
                        args: vec![arg],
                    });
                }
                Stmt::Return { value } => {
                    let lowered = match value {
                        Some(expr) => Some(self.lower_expr_value(
                            expr,
                            &mut entry,
                            &module_bindings,
                            &mut value_kinds,
                        )?),
                        None => None,
                    };
                    entry.terminator = Terminator::Return(lowered);
                    has_terminator = true;
                }
                Stmt::VarDecl {
                    name, initializer, ..
                } => {
                    let value = self.lower_expr_value(
                        initializer,
                        &mut entry,
                        &module_bindings,
                        &mut value_kinds,
                    )?;
                    let value_kind = self.kind_for_value(&value, &value_kinds);
                    entry.instructions.push(Instruction::Assign {
                        dest: name.clone(),
                        value,
                    });
                    value_kinds.insert(name.clone(), value_kind);
                }
                Stmt::Expr(expr) => {
                    self.lower_expression_statement(
                        expr,
                        &mut entry,
                        &module_bindings,
                        &mut value_kinds,
                    )?;
                }
                Stmt::Test { .. } => {
                    // Runtime only executes tests in explicit test mode. Ignore in normal lowering.
                }
                _ => {
                    return Err(CompileError::NotImplemented(
                        "this statement kind is not implemented in sculk lowering yet",
                    ));
                }
            }
        }

        if !has_terminator {
            entry.terminator = match return_type {
                Type::Int => Terminator::Return(Some(Value::Int(0))),
                _ => Terminator::Return(None),
            };
        }

        function.blocks.push(entry);
        Ok(function)
    }

    fn infer_return_type(
        &self,
        name: &str,
        declared_return_type: Option<&str>,
        body: &[Stmt],
    ) -> Type {
        if let Some(annotation) = declared_return_type {
            return self.lower_type_annotation(Some(annotation));
        }

        for stmt in body {
            if let Stmt::Return { value } = stmt {
                return match value {
                    None | Some(Expr::Nil) => Type::Void,
                    Some(Expr::Float(_)) => Type::Float,
                    Some(Expr::Bool(_)) => Type::Bool,
                    Some(Expr::String { .. }) => Type::String,
                    _ => Type::Int,
                };
            }
        }

        if name == "main" {
            Type::Int
        } else {
            Type::Void
        }
    }

    fn lower_expression_statement(
        &mut self,
        expr: &Expr,
        block: &mut BasicBlock,
        module_bindings: &HashMap<String, ModuleBinding>,
        value_kinds: &mut HashMap<String, LoweredValueKind>,
    ) -> Result<(), CompileError> {
        match expr {
            Expr::Call { callee, args } => {
                let call_target = self.resolve_call_target(callee, module_bindings)?;
                let mut lowered_args = Vec::with_capacity(args.len());
                for arg in args {
                    lowered_args.push(self.lower_expr_value(
                        arg,
                        block,
                        module_bindings,
                        value_kinds,
                    )?);
                }
                block.instructions.push(Instruction::Call {
                    dest: None,
                    func: call_target.func_name,
                    args: lowered_args,
                });
                Ok(())
            }
            Expr::Assign { name, value } => {
                let lowered = self.lower_expr_value(value, block, module_bindings, value_kinds)?;
                let value_kind = self.kind_for_value(&lowered, value_kinds);
                block.instructions.push(Instruction::Assign {
                    dest: name.clone(),
                    value: lowered,
                });
                value_kinds.insert(name.clone(), value_kind);
                Ok(())
            }
            _ => {
                let _ = self.lower_expr_value(expr, block, module_bindings, value_kinds)?;
                Ok(())
            }
        }
    }

    fn lower_expr_value(
        &mut self,
        expr: &Expr,
        block: &mut BasicBlock,
        module_bindings: &HashMap<String, ModuleBinding>,
        value_kinds: &mut HashMap<String, LoweredValueKind>,
    ) -> Result<Value, CompileError> {
        match expr {
            Expr::Int(value) => Ok(Value::Int(*value)),
            Expr::Float(value) => Ok(Value::Float(*value)),
            Expr::String { value, .. } => Ok(Value::String(value.clone())),
            Expr::Bool(value) => Ok(Value::Bool(*value)),
            Expr::Nil => Ok(Value::Null),
            Expr::Variable(name) => Ok(Value::Var(name.clone())),
            Expr::Grouping(inner) => {
                self.lower_expr_value(inner, block, module_bindings, value_kinds)
            }
            Expr::Binary { lhs, op, rhs } => {
                let left = self.lower_expr_value(lhs, block, module_bindings, value_kinds)?;
                let right = self.lower_expr_value(rhs, block, module_bindings, value_kinds)?;
                let dest = self.new_temp();
                let op = map_binary_op(*op)?;
                block.instructions.push(Instruction::BinOp {
                    dest: dest.clone(),
                    op,
                    left,
                    right,
                });
                value_kinds.insert(dest.clone(), LoweredValueKind::Unknown);
                Ok(Value::Var(dest))
            }
            Expr::Call { callee, args } => {
                let call_target = self.resolve_call_target(callee, module_bindings)?;
                let mut lowered_args = Vec::with_capacity(args.len());
                for arg in args {
                    lowered_args.push(self.lower_expr_value(
                        arg,
                        block,
                        module_bindings,
                        value_kinds,
                    )?);
                }
                let dest = self.new_temp();
                block.instructions.push(Instruction::Call {
                    dest: Some(dest.clone()),
                    func: call_target.func_name,
                    args: lowered_args,
                });
                value_kinds.insert(dest.clone(), call_target.result_kind);
                Ok(Value::Var(dest))
            }
            Expr::Member {
                object,
                property,
                optional,
            } => {
                if *optional {
                    return Err(CompileError::NotImplemented(
                        "optional member access is not implemented in sculk lowering yet",
                    ));
                }

                let object_value =
                    self.lower_expr_value(object, block, module_bindings, value_kinds)?;
                let object_kind = self.kind_for_value(&object_value, value_kinds);

                if object_kind == LoweredValueKind::HttpResponse && property == "status" {
                    let dest = self.new_temp();
                    block.instructions.push(Instruction::Call {
                        dest: Some(dest.clone()),
                        func: HTTP_RESPONSE_STATUS_FN.to_string(),
                        args: vec![object_value],
                    });
                    value_kinds.insert(dest.clone(), LoweredValueKind::Unknown);
                    Ok(Value::Var(dest))
                } else {
                    Err(CompileError::NotImplemented(
                        "this member access is not implemented in sculk lowering yet",
                    ))
                }
            }
            Expr::OrReturn { lhs, .. } => {
                // Minimal lowering for now: evaluate lhs and continue.
                self.lower_expr_value(lhs, block, module_bindings, value_kinds)
            }
            Expr::PanicUnwrap(inner) => {
                self.lower_expr_value(inner, block, module_bindings, value_kinds)
            }
            _ => Err(CompileError::NotImplemented(
                "this expression kind is not implemented in sculk lowering yet",
            )),
        }
    }

    fn register_use_import(
        &self,
        target: &UseTarget,
        alias: Option<&str>,
        module_bindings: &mut HashMap<String, ModuleBinding>,
    ) -> Result<(), CompileError> {
        match target {
            UseTarget::ModulePath(path)
                if path.len() == 2 && path[0] == "std" && path[1] == "http" =>
            {
                let binding_name = alias.unwrap_or("http");
                module_bindings.insert(binding_name.to_string(), ModuleBinding::StdHttp);
                Ok(())
            }
            UseTarget::ModulePath(_) => Ok(()),
            UseTarget::Url(_) => Err(CompileError::NotImplemented(
                "URL imports are not implemented in sculk lowering yet",
            )),
        }
    }

    fn resolve_call_target(
        &self,
        callee: &Expr,
        module_bindings: &HashMap<String, ModuleBinding>,
    ) -> Result<CallTarget, CompileError> {
        match callee {
            Expr::Variable(name) => Ok(CallTarget {
                func_name: name.clone(),
                result_kind: LoweredValueKind::Unknown,
            }),
            Expr::Member {
                object,
                property,
                optional,
            } => {
                if *optional {
                    return Err(CompileError::NotImplemented(
                        "optional method calls are not implemented in sculk lowering yet",
                    ));
                }

                if let Expr::Variable(object_name) = object.as_ref() {
                    if module_bindings.get(object_name) == Some(&ModuleBinding::StdHttp) {
                        let mapped = match property.as_str() {
                            "get" => (HTTP_GET_FN, LoweredValueKind::HttpResponse),
                            "post" => (HTTP_POST_FN, LoweredValueKind::HttpResponse),
                            "put" => (HTTP_PUT_FN, LoweredValueKind::HttpResponse),
                            "delete" => (HTTP_DELETE_FN, LoweredValueKind::HttpResponse),
                            _ => {
                                return Err(CompileError::NotImplemented(
                                    "unsupported std.http method in sculk lowering",
                                ));
                            }
                        };

                        return Ok(CallTarget {
                            func_name: mapped.0.to_string(),
                            result_kind: mapped.1,
                        });
                    }
                }

                Err(CompileError::NotImplemented(
                    "only direct function calls or supported std.http calls are implemented in sculk lowering",
                ))
            }
            _ => Err(CompileError::NotImplemented(
                "only direct function calls or supported std.http calls are implemented in sculk lowering",
            )),
        }
    }

    fn kind_for_ir_type(&self, ty: &Type) -> LoweredValueKind {
        match ty {
            Type::String | Type::Ptr(_) => LoweredValueKind::Unknown,
            Type::Int | Type::Bool | Type::Float | Type::Void | Type::Func { .. } => {
                LoweredValueKind::Unknown
            }
        }
    }

    fn lower_type_annotation(&self, annotation: Option<&str>) -> Type {
        let Some(annotation) = annotation else {
            return Type::Int;
        };

        let normalized = annotation
            .chars()
            .filter(|ch| !ch.is_whitespace())
            .collect::<String>()
            .to_ascii_lowercase();

        if normalized.contains("float") {
            Type::Float
        } else if normalized.contains("bool") {
            Type::Bool
        } else if normalized.contains("string") {
            Type::String
        } else if normalized == "nil" || normalized == "void" {
            Type::Void
        } else {
            Type::Int
        }
    }

    fn kind_for_value(
        &self,
        value: &Value,
        value_kinds: &HashMap<String, LoweredValueKind>,
    ) -> LoweredValueKind {
        match value {
            Value::Var(name) => value_kinds
                .get(name)
                .copied()
                .unwrap_or(LoweredValueKind::Unknown),
            _ => LoweredValueKind::Unknown,
        }
    }
}

impl Default for Lowering {
    fn default() -> Self {
        Self::new()
    }
}

fn map_binary_op(op: AstBinaryOp) -> Result<BinOp, CompileError> {
    let mapped = match op {
        AstBinaryOp::Add => BinOp::Add,
        AstBinaryOp::Subtract => BinOp::Sub,
        AstBinaryOp::Multiply => BinOp::Mul,
        AstBinaryOp::Divide => BinOp::Div,
        AstBinaryOp::Modulo => BinOp::Mod,
        AstBinaryOp::And => BinOp::And,
        AstBinaryOp::Equal => BinOp::Eq,
        AstBinaryOp::NotEqual => BinOp::Ne,
        AstBinaryOp::Less => BinOp::Lt,
        AstBinaryOp::LessEqual => BinOp::Le,
        AstBinaryOp::Greater => BinOp::Gt,
        AstBinaryOp::GreaterEqual => BinOp::Ge,
    };
    Ok(mapped)
}
