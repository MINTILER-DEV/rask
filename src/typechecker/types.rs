use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Nil,
    Error,
    List(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Function { params: Vec<Type>, ret: Box<Type> },
    Nullable(Box<Type>),
    Union(Vec<Type>),
    Result(Box<Type>, Box<Type>),
    Named { name: String, args: Vec<Type> },
    Unknown,
}

impl Type {
    pub fn parse(input: &str) -> Result<Self, TypeParseError> {
        TypeParser::new(input).parse()
    }

    pub fn normalize(self) -> Self {
        match self {
            Type::Union(types) => {
                let mut flattened: Vec<Type> = Vec::new();
                for ty in types {
                    let normalized = ty.normalize();
                    match normalized {
                        Type::Union(inner) => flattened.extend(inner),
                        other => flattened.push(other),
                    }
                }

                let mut deduped: Vec<Type> = Vec::new();
                for ty in flattened {
                    if !deduped.contains(&ty) {
                        deduped.push(ty);
                    }
                }
                if deduped.len() == 1 {
                    deduped.remove(0)
                } else {
                    Type::Union(deduped)
                }
            }
            Type::Nullable(inner) => match *inner {
                Type::Nullable(nested) => Type::Nullable(nested),
                Type::Union(mut items) => {
                    if !items.contains(&Type::Nil) {
                        items.push(Type::Nil);
                    }
                    Type::Union(items).normalize()
                }
                other => Type::Union(vec![other, Type::Nil]).normalize(),
            },
            Type::List(item) => Type::List(Box::new(item.normalize())),
            Type::Map(key, value) => {
                Type::Map(Box::new(key.normalize()), Box::new(value.normalize()))
            }
            Type::Function { params, ret } => Type::Function {
                params: params.into_iter().map(Type::normalize).collect(),
                ret: Box::new(ret.normalize()),
            },
            Type::Result(ok, err) => {
                Type::Result(Box::new(ok.normalize()), Box::new(err.normalize()))
            }
            Type::Named { name, args } => Type::Named {
                name,
                args: args.into_iter().map(Type::normalize).collect(),
            },
            other => other,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Nil => write!(f, "nil"),
            Type::Error => write!(f, "Error"),
            Type::List(inner) => write!(f, "List<{}>", inner),
            Type::Map(key, value) => write!(f, "Map<{},{}>", key, value),
            Type::Function { params, ret } => {
                let rendered = params
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({}) -> {}", rendered, ret)
            }
            Type::Nullable(inner) => write!(f, "{}?", inner),
            Type::Union(types) => {
                let rendered = types
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(" | ");
                write!(f, "{}", rendered)
            }
            Type::Result(ok, err) => write!(f, "Result<{},{}>", ok, err),
            Type::Named { name, args } => {
                if args.is_empty() {
                    write!(f, "{}", name)
                } else {
                    let rendered = args
                        .iter()
                        .map(|ty| ty.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{}<{}>", name, rendered)
                }
            }
            Type::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeParseError {
    pub message: String,
}

impl fmt::Display for TypeParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type parse error: {}", self.message)
    }
}

impl std::error::Error for TypeParseError {}

pub fn is_assignable(from: &Type, to: &Type) -> bool {
    if matches!(from, Type::Unknown) || matches!(to, Type::Unknown) {
        return true;
    }
    if from == to {
        return true;
    }

    match (from, to) {
        (_, Type::Union(options)) => options
            .iter()
            .any(|candidate| is_assignable(from, candidate)),
        (Type::Union(from_options), _) => from_options
            .iter()
            .all(|candidate| is_assignable(candidate, to)),
        (Type::Nil, Type::Nullable(_)) => true,
        (other, Type::Nullable(inner)) => is_assignable(other, inner),
        (Type::Int, Type::Float) => true,
        (Type::List(a), Type::List(b)) => is_assignable(a, b),
        (Type::Map(ka, va), Type::Map(kb, vb)) => is_assignable(ka, kb) && is_assignable(va, vb),
        (
            Type::Function {
                params: from_params,
                ret: from_ret,
            },
            Type::Function {
                params: to_params,
                ret: to_ret,
            },
        ) => {
            if from_params.len() != to_params.len() {
                return false;
            }
            for (from_param, to_param) in from_params.iter().zip(to_params.iter()) {
                if !is_assignable(to_param, from_param) {
                    return false;
                }
            }
            is_assignable(from_ret, to_ret)
        }
        (Type::Result(ok_a, err_a), Type::Result(ok_b, err_b)) => {
            is_assignable(ok_a, ok_b) && is_assignable(err_a, err_b)
        }
        (
            Type::Named {
                name: name_a,
                args: args_a,
            },
            Type::Named {
                name: name_b,
                args: args_b,
            },
        ) => {
            if name_a != name_b || args_a.len() != args_b.len() {
                return false;
            }
            args_a
                .iter()
                .zip(args_b.iter())
                .all(|(a, b)| is_assignable(a, b))
        }
        _ => false,
    }
}

pub fn common_numeric_type(lhs: &Type, rhs: &Type) -> Option<Type> {
    match (lhs, rhs) {
        (Type::Int, Type::Int) => Some(Type::Int),
        (Type::Float, Type::Float) => Some(Type::Float),
        (Type::Int, Type::Float) | (Type::Float, Type::Int) => Some(Type::Float),
        _ => None,
    }
}

struct TypeParser<'a> {
    chars: Vec<char>,
    current: usize,
    source: &'a str,
}

impl<'a> TypeParser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().collect(),
            current: 0,
            source,
        }
    }

    fn parse(mut self) -> Result<Type, TypeParseError> {
        self.skip_ws();
        if self.is_at_end() {
            return Err(self.error("empty type annotation"));
        }
        let ty = self.parse_union()?;
        self.skip_ws();
        if !self.is_at_end() {
            return Err(self.error("unexpected trailing characters in type annotation"));
        }
        Ok(ty.normalize())
    }

    fn parse_union(&mut self) -> Result<Type, TypeParseError> {
        let mut items = vec![self.parse_postfix_nullable()?];
        loop {
            self.skip_ws();
            if !self.matches('|') {
                break;
            }
            self.skip_ws();
            items.push(self.parse_postfix_nullable()?);
        }

        if items.len() == 1 {
            Ok(items.remove(0))
        } else {
            Ok(Type::Union(items).normalize())
        }
    }

    fn parse_postfix_nullable(&mut self) -> Result<Type, TypeParseError> {
        let mut ty = self.parse_primary()?;
        loop {
            self.skip_ws();
            if self.matches('?') {
                ty = Type::Nullable(Box::new(ty)).normalize();
                continue;
            }
            break;
        }
        Ok(ty)
    }

    fn parse_primary(&mut self) -> Result<Type, TypeParseError> {
        self.skip_ws();
        if self.matches('(') {
            let checkpoint = self.current;
            if let Some(function_ty) = self.try_parse_function_type(checkpoint)? {
                return Ok(function_ty);
            }

            self.current = checkpoint;
            let grouped = self.parse_union()?;
            self.skip_ws();
            self.expect(')', "expected ')' after grouped type")?;
            return Ok(grouped);
        }

        let ident = self.parse_identifier()?;
        self.skip_ws();
        let args = if self.matches('<') {
            self.parse_generic_args()?
        } else {
            Vec::new()
        };

        let named = match ident.as_str() {
            "int" => Type::Int,
            "float" => Type::Float,
            "string" => Type::String,
            "bool" => Type::Bool,
            "nil" => Type::Nil,
            "Error" => Type::Error,
            "List" => {
                if args.len() != 1 {
                    return Err(self.error("List expects 1 type argument"));
                }
                Type::List(Box::new(args[0].clone()))
            }
            "Map" => {
                if args.len() != 2 {
                    return Err(self.error("Map expects 2 type arguments"));
                }
                Type::Map(Box::new(args[0].clone()), Box::new(args[1].clone()))
            }
            "Result" => {
                if args.len() != 2 {
                    return Err(self.error("Result expects 2 type arguments"));
                }
                Type::Result(Box::new(args[0].clone()), Box::new(args[1].clone()))
            }
            _ => Type::Named { name: ident, args },
        };

        Ok(named)
    }

    fn try_parse_function_type(
        &mut self,
        checkpoint: usize,
    ) -> Result<Option<Type>, TypeParseError> {
        self.current = checkpoint;
        let mut params = Vec::new();
        self.skip_ws();
        if self.matches(')') {
            self.skip_ws();
            if !self.matches('-') || !self.matches('>') {
                return Ok(None);
            }
            self.skip_ws();
            let ret = self.parse_union()?;
            return Ok(Some(Type::Function {
                params,
                ret: Box::new(ret),
            }));
        }

        loop {
            let param = self.parse_union()?;
            params.push(param);
            self.skip_ws();
            if self.matches(',') {
                self.skip_ws();
                continue;
            }
            break;
        }

        self.expect(')', "expected ')' after function parameters")?;
        self.skip_ws();
        if !self.matches('-') || !self.matches('>') {
            return Ok(None);
        }
        self.skip_ws();
        let ret = self.parse_union()?;
        Ok(Some(Type::Function {
            params,
            ret: Box::new(ret),
        }))
    }

    fn parse_generic_args(&mut self) -> Result<Vec<Type>, TypeParseError> {
        let mut args = Vec::new();
        loop {
            self.skip_ws();
            args.push(self.parse_union()?);
            self.skip_ws();
            if self.matches(',') {
                continue;
            }
            self.expect('>', "expected '>' to close generic type arguments")?;
            break;
        }
        Ok(args)
    }

    fn parse_identifier(&mut self) -> Result<String, TypeParseError> {
        self.skip_ws();
        if self.is_at_end() {
            return Err(self.error("expected type name"));
        }

        let mut ident = String::new();
        let first = self.peek();
        if first == '_' || first.is_ascii_alphabetic() {
            ident.push(self.advance());
        } else {
            return Err(self.error("expected type name"));
        }

        while !self.is_at_end() {
            let c = self.peek();
            if c == '_' || c.is_ascii_alphanumeric() {
                ident.push(self.advance());
            } else {
                break;
            }
        }

        Ok(ident)
    }

    fn expect(&mut self, expected: char, message: &str) -> Result<(), TypeParseError> {
        if self.matches(expected) {
            Ok(())
        } else {
            Err(self.error(message))
        }
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn advance(&mut self) -> char {
        let c = self.chars[self.current];
        self.current += 1;
        c
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.chars[self.current]
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }

    fn skip_ws(&mut self) {
        while !self.is_at_end() && self.peek().is_ascii_whitespace() {
            self.current += 1;
        }
    }

    fn error(&self, message: impl Into<String>) -> TypeParseError {
        let _ = self.source;
        TypeParseError {
            message: message.into(),
        }
    }
}
