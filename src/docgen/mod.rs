use std::error::Error;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct DocGenReport {
    pub output_path: PathBuf,
    pub module_count: usize,
}

#[derive(Debug, Clone)]
pub struct DocGenError {
    pub message: String,
}

impl DocGenError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for DocGenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "doc generation error: {}", self.message)
    }
}

impl Error for DocGenError {}

#[derive(Debug, Clone)]
struct ModuleDoc {
    name: String,
    lines: Vec<String>,
}

pub fn generate_stdlib_reference(output_path: &Path) -> Result<DocGenReport, DocGenError> {
    let cwd = std::env::current_dir()
        .map_err(|err| DocGenError::new(format!("failed to read current directory: {}", err)))?;
    let stdlib_dir = cwd.join("stdlib").join("std");
    let modules = collect_module_docs(&stdlib_dir)?;

    if modules.is_empty() {
        return Err(DocGenError::new(format!(
            "no stdlib modules found in '{}'",
            stdlib_dir.display()
        )));
    }

    let content = render_markdown(&modules);
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent).map_err(|err| {
            DocGenError::new(format!(
                "failed to create docs output directory '{}': {}",
                parent.display(),
                err
            ))
        })?;
    }
    fs::write(output_path, content).map_err(|err| {
        DocGenError::new(format!(
            "failed to write generated docs '{}': {}",
            output_path.display(),
            err
        ))
    })?;

    Ok(DocGenReport {
        output_path: output_path.to_path_buf(),
        module_count: modules.len(),
    })
}

fn collect_module_docs(stdlib_dir: &Path) -> Result<Vec<ModuleDoc>, DocGenError> {
    let read_dir = fs::read_dir(stdlib_dir).map_err(|err| {
        DocGenError::new(format!(
            "failed to read stdlib directory '{}': {}",
            stdlib_dir.display(),
            err
        ))
    })?;

    let mut files = Vec::new();
    for entry in read_dir {
        let entry =
            entry.map_err(|err| DocGenError::new(format!("failed to read dir entry: {}", err)))?;
        let path = entry.path();
        if path.extension().and_then(|v| v.to_str()) == Some("rask") {
            files.push(path);
        }
    }

    files.sort_by(|a, b| {
        a.file_name()
            .unwrap_or_default()
            .cmp(b.file_name().unwrap_or_default())
    });

    let mut modules = Vec::new();
    for path in files {
        let name = path
            .file_stem()
            .and_then(|v| v.to_str())
            .ok_or_else(|| {
                DocGenError::new(format!(
                    "invalid stdlib module filename '{}'",
                    path.display()
                ))
            })?
            .to_string();
        let source = fs::read_to_string(&path).map_err(|err| {
            DocGenError::new(format!(
                "failed to read stdlib module '{}': {}",
                path.display(),
                err
            ))
        })?;
        let lines = extract_top_doc_lines(&source);
        modules.push(ModuleDoc { name, lines });
    }
    Ok(modules)
}

fn extract_top_doc_lines(source: &str) -> Vec<String> {
    let mut lines = Vec::new();
    let mut in_block = false;

    for raw in source.lines() {
        let trimmed = raw.trim_start();
        if trimmed.starts_with('#') {
            in_block = true;
            let doc = trimmed.trim_start_matches('#').trim_start().to_string();
            lines.push(doc);
            continue;
        }

        if !in_block && trimmed.is_empty() {
            continue;
        }
        break;
    }

    while lines.first().is_some_and(|line| line.is_empty()) {
        lines.remove(0);
    }
    while lines.last().is_some_and(|line| line.is_empty()) {
        lines.pop();
    }
    lines
}

fn render_markdown(modules: &[ModuleDoc]) -> String {
    let mut out = String::new();
    out.push_str("# Standard Library Reference (Generated)\n\n");
    out.push_str("This file is auto-generated from top-of-file comments in `stdlib/std/*.rask`.\n");
    out.push_str("Regenerate with `rask docs` (or `cargo run -- docs`).\n\n");

    out.push_str("## Modules\n\n");
    for module in modules {
        out.push_str(&format!("### `std.{}`\n", module.name));
        if module.lines.is_empty() {
            out.push_str("_No module docs found._\n\n");
            continue;
        }
        for line in &module.lines {
            if line.is_empty() {
                out.push('\n');
            } else {
                out.push_str(line);
                out.push('\n');
            }
        }
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::extract_top_doc_lines;

    #[test]
    fn extracts_top_comment_block() {
        let source = "# A\n#\n# - item\nvalue = 1\n# not included";
        let lines = extract_top_doc_lines(source);
        assert_eq!(lines, vec!["A", "", "- item"]);
    }

    #[test]
    fn skips_leading_blank_lines() {
        let source = "\n\n# A\n# B\n";
        let lines = extract_top_doc_lines(source);
        assert_eq!(lines, vec!["A", "B"]);
    }
}
