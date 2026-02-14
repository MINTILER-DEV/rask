#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Use {
        target: UseTarget,
        alias: Option<String>,
    },
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
    For {
        initializer: Option<Box<Stmt>>,
        condition: Option<Expr>,
        increment: Option<Expr>,
        body: Vec<Stmt>,
    },
    ForIn {
        item_name: String,
        iterable: Expr,
        body: Vec<Stmt>,
    },
    Test {
        name: String,
        body: Vec<Stmt>,
    },
    Assert {
        condition: Expr,
        message: Option<Expr>,
    },
    VarDecl {
        name: String,
        type_annotation: Option<String>,
        initializer: Expr,
    },
    DestructureDecl {
        pattern: Pattern,
        initializer: Expr,
    },
    FunctionDef {
        name: String,
        params: Vec<Param>,
        return_type: Option<String>,
        body: Vec<Stmt>,
    },
    Return {
        value: Option<Expr>,
    },
    Print {
        expr: Expr,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UseTarget {
    ModulePath(Vec<String>),
    Url(String),
}

impl UseTarget {
    pub fn default_binding_name(&self) -> String {
        match self {
            UseTarget::ModulePath(path) => {
                path.last().cloned().unwrap_or_else(|| "module".to_string())
            }
            UseTarget::Url(spec) => default_name_for_url_import(spec),
        }
    }
}

fn default_name_for_url_import(spec: &str) -> String {
    let base = spec
        .split('/')
        .next_back()
        .unwrap_or("module")
        .split('@')
        .next()
        .unwrap_or("module")
        .trim_end_matches(".scl");

    let mut out = String::new();
    for ch in base.chars() {
        if ch == '_' || ch.is_ascii_alphanumeric() {
            out.push(ch);
        } else {
            out.push('_');
        }
    }

    if out.is_empty() {
        "module".to_string()
    } else if out.chars().next().unwrap_or('m').is_ascii_digit() {
        format!("m_{}", out)
    } else {
        out
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub type_annotation: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Float(f64),
    String {
        value: String,
        has_interpolation: bool,
    },
    Bool(bool),
    Nil,
    Variable(String),
    Unary {
        op: UnaryOp,
        rhs: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
    },
    Assign {
        name: String,
        value: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Member {
        object: Box<Expr>,
        property: String,
        optional: bool,
    },
    Coalesce {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    OrReturn {
        lhs: Box<Expr>,
        return_value: Box<Expr>,
    },
    PanicUnwrap(Box<Expr>),
    Match {
        subject: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    ListLiteral(Vec<Expr>),
    ListComprehension {
        expr: Box<Expr>,
        item_name: String,
        iterable: Box<Expr>,
        condition: Option<Box<Expr>>,
    },
    MapLiteral(Vec<MapEntryExpr>),
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
    },
    Function {
        params: Vec<Param>,
        return_type: Option<String>,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapEntryExpr {
    pub key: String,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Identifier(String),
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Nil,
    List(Vec<Pattern>),
    Map(Vec<MapPatternEntry>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapPatternEntry {
    pub key: String,
    pub pattern: Pattern,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}
