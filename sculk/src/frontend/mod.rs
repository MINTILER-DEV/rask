//! Frontend utilities for parsing and import expansion.

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use reqwest::blocking::Client;
use scalf::parser::ast::{Program, Stmt, UseTarget};

use crate::CompileError;

/// Parses SCALF source and expands supported `use` statements.
#[derive(Debug, Default)]
pub struct Frontend {
    loaded_files: HashSet<PathBuf>,
    loaded_url_specs: HashSet<String>,
    import_stack: Vec<PathBuf>,
    url_import_stack: Vec<String>,
}

impl Frontend {
    /// Create a new frontend instance.
    pub fn new() -> Self {
        Self::default()
    }

    /// Parse source and expand all supported imports.
    pub fn parse_and_expand(
        &mut self,
        source: &str,
        origin_path: Option<&Path>,
    ) -> Result<Program, CompileError> {
        let base_dir = origin_path
            .map(resolve_base_dir)
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));
        let parsed = parse_program(source)?;
        let statements = self.expand_statements(&parsed.statements, &base_dir)?;
        Ok(Program { statements })
    }

    fn expand_statements(
        &mut self,
        statements: &[Stmt],
        base_dir: &Path,
    ) -> Result<Vec<Stmt>, CompileError> {
        let mut out = Vec::new();
        for stmt in statements {
            match stmt {
                Stmt::Use { target, alias } => {
                    if matches!(target, UseTarget::ModulePath(_)) {
                        // Module-path imports are handled later by lowering/runtime bindings.
                        out.push(stmt.clone());
                    } else {
                        let imported = self.resolve_use(target, alias.as_deref(), base_dir)?;
                        out.extend(imported);
                    }
                }
                _ => out.push(stmt.clone()),
            }
        }
        Ok(out)
    }

    fn resolve_use(
        &mut self,
        target: &UseTarget,
        alias: Option<&str>,
        base_dir: &Path,
    ) -> Result<Vec<Stmt>, CompileError> {
        match target {
            UseTarget::ModulePath(_) => Ok(vec![Stmt::Use {
                target: target.clone(),
                alias: alias.map(str::to_string),
            }]),
            UseTarget::Url(spec) => {
                if is_http_import_spec(spec) {
                    // URL imports are expanded inline in the frontend.
                    let _ = alias;
                    return self.resolve_url_use(spec, base_dir);
                }

                if alias.is_some() {
                    return Err(CompileError::NotImplemented(
                        "`use ... as ...` is not implemented in sculk yet",
                    ));
                }

                self.resolve_local_file_use(spec, base_dir)
            }
        }
    }

    fn resolve_local_file_use(
        &mut self,
        spec: &str,
        base_dir: &Path,
    ) -> Result<Vec<Stmt>, CompileError> {
        let resolved = resolve_local_import_path(spec, base_dir)?;

        if self.import_stack.contains(&resolved) {
            let mut chain = self
                .import_stack
                .iter()
                .map(|p| p.display().to_string())
                .collect::<Vec<_>>();
            chain.push(resolved.display().to_string());
            return Err(CompileError::FrontendError(format!(
                "circular local import detected: {}",
                chain.join(" -> ")
            )));
        }

        if self.loaded_files.contains(&resolved) {
            return Ok(Vec::new());
        }

        let source = fs::read_to_string(&resolved).map_err(|err| {
            CompileError::FrontendError(format!(
                "failed to read import '{}': {}",
                resolved.display(),
                err
            ))
        })?;

        self.import_stack.push(resolved.clone());
        let result = (|| {
            let parsed = parse_program(&source)?;
            let imported_base_dir = resolve_base_dir(&resolved);
            self.expand_statements(&parsed.statements, &imported_base_dir)
        })();
        self.import_stack.pop();

        let statements = result?;
        self.loaded_files.insert(resolved);
        Ok(statements)
    }

    fn resolve_url_use(&mut self, spec: &str, base_dir: &Path) -> Result<Vec<Stmt>, CompileError> {
        let spec_key = spec.trim().to_string();
        if self.url_import_stack.iter().any(|entry| entry == &spec_key) {
            let mut chain = self.url_import_stack.clone();
            chain.push(spec_key);
            return Err(CompileError::FrontendError(format!(
                "circular URL import detected: {}",
                chain.join(" -> ")
            )));
        }

        if self.loaded_url_specs.contains(&spec_key) {
            return Ok(Vec::new());
        }

        let (fetch_url, _version_hint) = parse_url_import_spec(spec)?;
        let source = fetch_url_source(&fetch_url)?;

        self.url_import_stack.push(spec_key.clone());
        let result = (|| {
            let parsed = parse_program(&source)?;
            self.expand_statements(&parsed.statements, base_dir)
        })();
        self.url_import_stack.pop();

        let statements = result?;
        self.loaded_url_specs.insert(spec_key);
        Ok(statements)
    }
}

fn parse_program(source: &str) -> Result<Program, CompileError> {
    let tokens = scalf::lexer::lex(source)
        .map_err(|err| CompileError::FrontendError(format!("lex error: {}", err)))?;
    let mut parser = scalf::parser::Parser::new(tokens);
    parser
        .parse_program()
        .map_err(|err| CompileError::FrontendError(format!("parse error: {}", err)))
}

fn resolve_local_import_path(spec: &str, base_dir: &Path) -> Result<PathBuf, CompileError> {
    let candidate = PathBuf::from(spec);
    let joined = if candidate.is_absolute() {
        candidate
    } else {
        base_dir.join(candidate)
    };
    let normalized = normalize_path(joined);

    if !normalized.exists() {
        return Err(CompileError::FrontendError(format!(
            "import not found: '{}'",
            normalized.display()
        )));
    }

    std::fs::canonicalize(&normalized).map_err(|err| {
        CompileError::FrontendError(format!(
            "failed to canonicalize import '{}': {}",
            normalized.display(),
            err
        ))
    })
}

fn resolve_base_dir(origin_path: &Path) -> PathBuf {
    if origin_path.as_os_str().is_empty() {
        return std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    }

    let normalized = normalize_path(origin_path.to_path_buf());
    if normalized.is_dir() {
        normalized
    } else {
        normalized
            .parent()
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("."))
    }
}

fn normalize_path(path: PathBuf) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                normalized.pop();
            }
            _ => normalized.push(component.as_os_str()),
        }
    }
    normalized
}

fn is_http_import_spec(spec: &str) -> bool {
    let lowered = spec.trim().to_ascii_lowercase();
    lowered.starts_with("http://") || lowered.starts_with("https://")
}

fn parse_url_import_spec(spec: &str) -> Result<(String, Option<String>), CompileError> {
    let trimmed = spec.trim();
    if !is_http_import_spec(trimmed) {
        return Err(CompileError::FrontendError(format!(
            "URL imports must start with http:// or https://, got '{}'",
            spec
        )));
    }

    if let Some((url, hint)) = trimmed.rsplit_once('@') {
        if is_http_import_spec(url) {
            let version_hint = if hint.trim().is_empty() {
                None
            } else {
                Some(hint.trim().to_string())
            };
            return Ok((url.to_string(), version_hint));
        }
    }

    Ok((trimmed.to_string(), None))
}

fn fetch_url_source(url: &str) -> Result<String, CompileError> {
    let client = Client::builder().build().map_err(|err| {
        CompileError::FrontendError(format!("failed to initialize HTTP client: {}", err))
    })?;

    let response = client.get(url).send().map_err(|err| {
        CompileError::FrontendError(format!("failed to fetch URL import '{}': {}", url, err))
    })?;

    let status = response.status();
    if !status.is_success() {
        return Err(CompileError::FrontendError(format!(
            "URL import '{}' returned HTTP {}",
            url, status
        )));
    }

    response.text().map_err(|err| {
        CompileError::FrontendError(format!("failed to read URL import body '{}': {}", url, err))
    })
}
