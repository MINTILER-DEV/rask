//! IR optimization passes
//!
//! Optimizations are applied to IR before code generation.
//! Each pass transforms the IR to make it faster/smaller.

use std::collections::{HashMap, HashSet, VecDeque};

use crate::ir::{BinOp, Function, Instruction, Module, Terminator, Value};
use crate::CompileError;

/// Optimization pass trait
pub trait Pass {
    /// Run the optimization pass on a module
    fn run(&self, module: &mut Module) -> Result<(), CompileError>;

    /// Get pass name
    fn name(&self) -> &'static str;
}

/// Pass manager - runs optimization passes in order
pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl PassManager {
    /// Create a new pass manager
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    /// Add an optimization pass
    pub fn add_pass(&mut self, pass: Box<dyn Pass>) {
        self.passes.push(pass);
    }

    /// Run all passes on a module
    pub fn run(&self, module: &mut Module) -> Result<(), CompileError> {
        for pass in &self.passes {
            pass.run(module)?;
        }
        Ok(())
    }
}

impl Default for PassManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Constant folding pass
pub struct ConstantFolding;

impl Pass for ConstantFolding {
    fn run(&self, module: &mut Module) -> Result<(), CompileError> {
        for function in &mut module.functions {
            for block in &mut function.blocks {
                let mut constants: HashMap<String, Value> = HashMap::new();

                for instruction in &mut block.instructions {
                    match instruction {
                        Instruction::Assign { dest, value } => {
                            *value = substitute_value(value, &constants);
                            if is_literal(value) {
                                constants.insert(dest.clone(), value.clone());
                            } else {
                                constants.remove(dest);
                            }
                        }
                        Instruction::BinOp {
                            dest,
                            op,
                            left,
                            right,
                        } => {
                            *left = substitute_value(left, &constants);
                            *right = substitute_value(right, &constants);

                            if let Some(folded) = eval_binop(*op, left, right) {
                                let dest_name = dest.clone();
                                *instruction = Instruction::Assign {
                                    dest: dest_name.clone(),
                                    value: folded.clone(),
                                };
                                constants.insert(dest_name, folded);
                            } else {
                                constants.remove(dest);
                            }
                        }
                        Instruction::Call { dest, args, .. } => {
                            for arg in args {
                                *arg = substitute_value(arg, &constants);
                            }
                            if let Some(dest) = dest {
                                constants.remove(dest);
                            }
                        }
                        Instruction::Load { dest, addr } => {
                            *addr = substitute_value(addr, &constants);
                            constants.remove(dest);
                        }
                        Instruction::Store { addr, value } => {
                            *addr = substitute_value(addr, &constants);
                            *value = substitute_value(value, &constants);
                        }
                    }
                }

                let folded_branch_target = match &mut block.terminator {
                    Terminator::Return(Some(value)) => {
                        *value = substitute_value(value, &constants);
                        None
                    }
                    Terminator::Return(None)
                    | Terminator::Branch { .. }
                    | Terminator::Unreachable => None,
                    Terminator::CondBranch {
                        cond,
                        then_block,
                        else_block,
                    } => {
                        *cond = substitute_value(cond, &constants);
                        const_truthiness(cond)
                            .map(|is_truthy| if is_truthy { then_block } else { else_block })
                            .cloned()
                    }
                };

                if let Some(target) = folded_branch_target {
                    block.terminator = Terminator::Branch { target };
                }
            }
        }

        Ok(())
    }

    fn name(&self) -> &'static str {
        "constant-folding"
    }
}

/// Dead code elimination pass
pub struct DeadCodeElimination;

impl Pass for DeadCodeElimination {
    fn run(&self, module: &mut Module) -> Result<(), CompileError> {
        eliminate_unused_functions(module);

        for function in &mut module.functions {
            eliminate_unreachable_blocks(function);
            for block in &mut function.blocks {
                eliminate_dead_instructions(block);
            }
        }

        Ok(())
    }

    fn name(&self) -> &'static str {
        "dead-code-elimination"
    }
}

/// Function inlining pass
pub struct Inlining;

impl Pass for Inlining {
    fn run(&self, module: &mut Module) -> Result<(), CompileError> {
        let inlineable_returns = module
            .functions
            .iter()
            .filter_map(|function| {
                inlineable_constant_return(function).map(|value| (function.name.clone(), value))
            })
            .collect::<HashMap<_, _>>();

        for function in &mut module.functions {
            for block in &mut function.blocks {
                let mut rewritten = Vec::with_capacity(block.instructions.len());

                for instruction in block.instructions.drain(..) {
                    match instruction {
                        Instruction::Call { dest, func, args }
                            if args.is_empty() && inlineable_returns.contains_key(&func) =>
                        {
                            if let Some(dest) = dest {
                                rewritten.push(Instruction::Assign {
                                    dest,
                                    value: inlineable_returns
                                        .get(&func)
                                        .expect("key existence checked")
                                        .clone(),
                                });
                            }
                        }
                        other => rewritten.push(other),
                    }
                }

                block.instructions = rewritten;
            }
        }

        Ok(())
    }

    fn name(&self) -> &'static str {
        "inlining"
    }
}

fn is_literal(value: &Value) -> bool {
    !matches!(value, Value::Var(_))
}

fn substitute_value(value: &Value, constants: &HashMap<String, Value>) -> Value {
    match value {
        Value::Var(name) => constants
            .get(name)
            .cloned()
            .unwrap_or_else(|| Value::Var(name.clone())),
        _ => value.clone(),
    }
}

fn eval_binop(op: BinOp, left: &Value, right: &Value) -> Option<Value> {
    match op {
        BinOp::Add => match (left, right) {
            (Value::Int(a), Value::Int(b)) => Some(Value::Int(a + b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a + b)),
            (Value::Int(a), Value::Float(b)) => Some(Value::Float((*a as f64) + b)),
            (Value::Float(a), Value::Int(b)) => Some(Value::Float(a + (*b as f64))),
            _ => None,
        },
        BinOp::Sub => match (left, right) {
            (Value::Int(a), Value::Int(b)) => Some(Value::Int(a - b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a - b)),
            (Value::Int(a), Value::Float(b)) => Some(Value::Float((*a as f64) - b)),
            (Value::Float(a), Value::Int(b)) => Some(Value::Float(a - (*b as f64))),
            _ => None,
        },
        BinOp::Mul => match (left, right) {
            (Value::Int(a), Value::Int(b)) => Some(Value::Int(a * b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a * b)),
            (Value::Int(a), Value::Float(b)) => Some(Value::Float((*a as f64) * b)),
            (Value::Float(a), Value::Int(b)) => Some(Value::Float(a * (*b as f64))),
            _ => None,
        },
        BinOp::Div => match (left, right) {
            (Value::Int(_), Value::Int(0)) => None,
            (Value::Int(a), Value::Int(b)) => Some(Value::Int(a / b)),
            (Value::Float(_), Value::Float(b)) if *b == 0.0 => None,
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a / b)),
            (Value::Int(_), Value::Float(b)) if *b == 0.0 => None,
            (Value::Int(a), Value::Float(b)) => Some(Value::Float((*a as f64) / b)),
            (Value::Float(_), Value::Int(0)) => None,
            (Value::Float(a), Value::Int(b)) => Some(Value::Float(a / (*b as f64))),
            _ => None,
        },
        BinOp::Mod => match (left, right) {
            (Value::Int(_), Value::Int(0)) => None,
            (Value::Int(a), Value::Int(b)) => Some(Value::Int(a % b)),
            _ => None,
        },
        BinOp::Eq => eval_eq(left, right).map(Value::Bool),
        BinOp::Ne => eval_eq(left, right).map(|value| Value::Bool(!value)),
        BinOp::Lt => eval_cmp(left, right, |a, b| a < b).map(Value::Bool),
        BinOp::Le => eval_cmp(left, right, |a, b| a <= b).map(Value::Bool),
        BinOp::Gt => eval_cmp(left, right, |a, b| a > b).map(Value::Bool),
        BinOp::Ge => eval_cmp(left, right, |a, b| a >= b).map(Value::Bool),
        BinOp::And => Some(Value::Bool(
            const_truthiness(left)? && const_truthiness(right)?,
        )),
        BinOp::Or => Some(Value::Bool(
            const_truthiness(left)? || const_truthiness(right)?,
        )),
    }
}

fn eval_eq(left: &Value, right: &Value) -> Option<bool> {
    let value = match (left, right) {
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Int(a), Value::Float(b)) => (*a as f64) == *b,
        (Value::Float(a), Value::Int(b)) => *a == (*b as f64),
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Null, Value::Null) => true,
        _ => return None,
    };
    Some(value)
}

fn eval_cmp(left: &Value, right: &Value, compare: impl FnOnce(f64, f64) -> bool) -> Option<bool> {
    let (lhs, rhs) = match (left, right) {
        (Value::Int(a), Value::Int(b)) => (*a as f64, *b as f64),
        (Value::Float(a), Value::Float(b)) => (*a, *b),
        (Value::Int(a), Value::Float(b)) => (*a as f64, *b),
        (Value::Float(a), Value::Int(b)) => (*a, *b as f64),
        _ => return None,
    };

    Some(compare(lhs, rhs))
}

fn const_truthiness(value: &Value) -> Option<bool> {
    match value {
        Value::Bool(value) => Some(*value),
        Value::Int(value) => Some(*value != 0),
        Value::Float(value) => Some(*value != 0.0),
        Value::String(value) => Some(!value.is_empty()),
        Value::Null => Some(false),
        Value::Var(_) => None,
    }
}

fn inlineable_constant_return(function: &Function) -> Option<Value> {
    if !function.params.is_empty() || function.blocks.len() != 1 {
        return None;
    }

    let block = &function.blocks[0];
    if !block.instructions.is_empty() {
        return None;
    }

    match &block.terminator {
        Terminator::Return(Some(value)) if is_literal(value) => Some(value.clone()),
        _ => None,
    }
}

fn eliminate_unused_functions(module: &mut Module) {
    if !module
        .functions
        .iter()
        .any(|function| function.name == "main")
    {
        return;
    }

    let mut index_by_name = HashMap::new();
    for (index, function) in module.functions.iter().enumerate() {
        index_by_name.insert(function.name.clone(), index);
    }

    let mut reachable = HashSet::new();
    let mut queue = VecDeque::new();
    reachable.insert("main".to_string());
    queue.push_back("main".to_string());

    while let Some(function_name) = queue.pop_front() {
        let Some(index) = index_by_name.get(&function_name).copied() else {
            continue;
        };

        for block in &module.functions[index].blocks {
            for instruction in &block.instructions {
                if let Instruction::Call { func, .. } = instruction {
                    if index_by_name.contains_key(func) && reachable.insert(func.clone()) {
                        queue.push_back(func.clone());
                    }
                }
            }
        }
    }

    module
        .functions
        .retain(|function| reachable.contains(&function.name));
}

fn eliminate_unreachable_blocks(function: &mut Function) {
    let Some(entry_label) = function.blocks.first().map(|block| block.label.clone()) else {
        return;
    };

    let mut index_by_label = HashMap::new();
    for (index, block) in function.blocks.iter().enumerate() {
        index_by_label.insert(block.label.clone(), index);
    }

    let mut reachable = HashSet::new();
    let mut stack = vec![entry_label];

    while let Some(label) = stack.pop() {
        if !reachable.insert(label.clone()) {
            continue;
        }

        let Some(index) = index_by_label.get(&label).copied() else {
            continue;
        };

        match &function.blocks[index].terminator {
            Terminator::Branch { target } => stack.push(target.clone()),
            Terminator::CondBranch {
                then_block,
                else_block,
                ..
            } => {
                stack.push(then_block.clone());
                stack.push(else_block.clone());
            }
            Terminator::Return(_) | Terminator::Unreachable => {}
        }
    }

    function
        .blocks
        .retain(|block| reachable.contains(&block.label));
}

fn eliminate_dead_instructions(block: &mut crate::ir::BasicBlock) {
    let mut live = HashSet::new();
    collect_live_from_terminator(&block.terminator, &mut live);

    let mut kept = Vec::with_capacity(block.instructions.len());

    for instruction in block.instructions.iter().rev() {
        match instruction {
            Instruction::Assign { dest, value } => {
                if !live.contains(dest) {
                    continue;
                }
                live.remove(dest);
                collect_vars_from_value(value, &mut live);
                kept.push(instruction.clone());
            }
            Instruction::BinOp {
                dest, left, right, ..
            } => {
                if !live.contains(dest) {
                    continue;
                }
                live.remove(dest);
                collect_vars_from_value(left, &mut live);
                collect_vars_from_value(right, &mut live);
                kept.push(instruction.clone());
            }
            Instruction::Call { dest, args, .. } => {
                if let Some(dest) = dest {
                    live.remove(dest);
                }
                for arg in args {
                    collect_vars_from_value(arg, &mut live);
                }
                kept.push(instruction.clone());
            }
            Instruction::Load { dest, addr } => {
                live.remove(dest);
                collect_vars_from_value(addr, &mut live);
                kept.push(instruction.clone());
            }
            Instruction::Store { addr, value } => {
                collect_vars_from_value(addr, &mut live);
                collect_vars_from_value(value, &mut live);
                kept.push(instruction.clone());
            }
        }
    }

    kept.reverse();
    block.instructions = kept;
}

fn collect_live_from_terminator(terminator: &Terminator, vars: &mut HashSet<String>) {
    match terminator {
        Terminator::Return(Some(value)) => collect_vars_from_value(value, vars),
        Terminator::CondBranch { cond, .. } => collect_vars_from_value(cond, vars),
        Terminator::Return(None) | Terminator::Branch { .. } | Terminator::Unreachable => {}
    }
}

fn collect_vars_from_value(value: &Value, vars: &mut HashSet<String>) {
    if let Value::Var(name) = value {
        vars.insert(name.clone());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Module, Parameter, Type};

    fn single_block_function(name: &str) -> Function {
        Function {
            name: name.to_string(),
            params: Vec::<Parameter>::new(),
            return_type: Type::Int,
            blocks: vec![BasicBlock {
                label: "entry".to_string(),
                instructions: Vec::new(),
                terminator: Terminator::Return(None),
            }],
        }
    }

    #[test]
    fn constant_folding_rewrites_binop_and_return() {
        let mut function = single_block_function("main");
        function.blocks[0].instructions.push(Instruction::BinOp {
            dest: "tmp".to_string(),
            op: BinOp::Add,
            left: Value::Int(2),
            right: Value::Int(3),
        });
        function.blocks[0].terminator = Terminator::Return(Some(Value::Var("tmp".to_string())));

        let mut module = Module::new("fold".to_string());
        module.functions.push(function);

        ConstantFolding
            .run(&mut module)
            .expect("constant folding should succeed");

        let block = &module.functions[0].blocks[0];
        assert!(matches!(
            block.instructions.first(),
            Some(Instruction::Assign {
                dest,
                value: Value::Int(5)
            }) if dest == "tmp"
        ));
        assert!(matches!(
            block.terminator,
            Terminator::Return(Some(Value::Int(5)))
        ));
    }

    #[test]
    fn dead_code_elimination_removes_unused_assignments() {
        let mut function = single_block_function("main");
        function.blocks[0].instructions.push(Instruction::Assign {
            dest: "dead".to_string(),
            value: Value::Int(1),
        });
        function.blocks[0].instructions.push(Instruction::Assign {
            dest: "live".to_string(),
            value: Value::Int(2),
        });
        function.blocks[0].terminator = Terminator::Return(Some(Value::Var("live".to_string())));

        let mut module = Module::new("dce".to_string());
        module.functions.push(function);

        DeadCodeElimination
            .run(&mut module)
            .expect("dead code elimination should succeed");

        let block = &module.functions[0].blocks[0];
        assert_eq!(block.instructions.len(), 1);
        assert!(matches!(
            block.instructions.first(),
            Some(Instruction::Assign { dest, .. }) if dest == "live"
        ));
    }
}
