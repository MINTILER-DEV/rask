pub fn normalize_line(input: &str) -> String {
    input.trim_end().to_string()
}

const ANSI_RESET: &str = "\x1b[0m";
const ANSI_KEYWORD: &str = "\x1b[94m";
const ANSI_STRING: &str = "\x1b[92m";
const ANSI_NUMBER: &str = "\x1b[93m";
const ANSI_COMMENT: &str = "\x1b[90m";

const KEYWORDS: &[&str] = &[
    "def", "print", "if", "else", "while", "for", "return", "or", "and", "match", "use", "true",
    "false", "nil", "test", "assert", "in",
];

pub fn needs_more_input(source: &str) -> bool {
    let mut paren_depth: i32 = 0;
    let mut brace_depth: i32 = 0;
    let mut bracket_depth: i32 = 0;
    let mut in_string = false;
    let mut escaped = false;
    let mut in_block_comment = false;
    let mut chars = source.chars().peekable();

    while let Some(ch) = chars.next() {
        if in_block_comment {
            if ch == '*' && chars.peek() == Some(&'/') {
                let _ = chars.next();
                in_block_comment = false;
            }
            continue;
        }

        if in_string {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }

        if ch == '#' {
            while let Some(next) = chars.peek() {
                if *next == '\n' {
                    break;
                }
                let _ = chars.next();
            }
            continue;
        }

        if ch == '/' && chars.peek() == Some(&'/') {
            while let Some(next) = chars.peek() {
                if *next == '\n' {
                    break;
                }
                let _ = chars.next();
            }
            continue;
        }

        if ch == '/' && chars.peek() == Some(&'*') {
            let _ = chars.next();
            in_block_comment = true;
            continue;
        }

        match ch {
            '"' => in_string = true,
            '(' => paren_depth += 1,
            ')' => paren_depth -= 1,
            '{' => brace_depth += 1,
            '}' => brace_depth -= 1,
            '[' => bracket_depth += 1,
            ']' => bracket_depth -= 1,
            _ => {}
        }
    }

    if in_string || in_block_comment {
        return true;
    }
    if paren_depth > 0 || brace_depth > 0 || bracket_depth > 0 {
        return true;
    }

    let trimmed = source.trim_end();
    if trimmed.is_empty() {
        return false;
    }
    trimmed.ends_with('+')
        || trimmed.ends_with('-')
        || trimmed.ends_with('*')
        || trimmed.ends_with('/')
        || trimmed.ends_with('%')
        || trimmed.ends_with('=')
        || trimmed.ends_with("->")
        || trimmed.ends_with(',')
}

pub fn colorize(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 16);
    let chars = input.chars().collect::<Vec<_>>();
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];

        if ch == '#' {
            out.push_str(ANSI_COMMENT);
            while i < chars.len() {
                out.push(chars[i]);
                if chars[i] == '\n' {
                    break;
                }
                i += 1;
            }
            out.push_str(ANSI_RESET);
            i += 1;
            continue;
        }

        if ch == '"' {
            out.push_str(ANSI_STRING);
            out.push(ch);
            i += 1;
            let mut escaped = false;
            while i < chars.len() {
                let current = chars[i];
                out.push(current);
                if escaped {
                    escaped = false;
                } else if current == '\\' {
                    escaped = true;
                } else if current == '"' {
                    i += 1;
                    break;
                }
                i += 1;
            }
            out.push_str(ANSI_RESET);
            continue;
        }

        if ch.is_ascii_digit() {
            out.push_str(ANSI_NUMBER);
            out.push(ch);
            i += 1;
            while i < chars.len() && (chars[i].is_ascii_digit() || chars[i] == '.') {
                out.push(chars[i]);
                i += 1;
            }
            out.push_str(ANSI_RESET);
            continue;
        }

        if ch == '_' || ch.is_ascii_alphabetic() {
            let start = i;
            i += 1;
            while i < chars.len() && (chars[i] == '_' || chars[i].is_ascii_alphanumeric()) {
                i += 1;
            }
            let word = chars[start..i].iter().collect::<String>();
            if KEYWORDS.contains(&word.as_str()) {
                out.push_str(ANSI_KEYWORD);
                out.push_str(&word);
                out.push_str(ANSI_RESET);
            } else {
                out.push_str(&word);
            }
            continue;
        }

        out.push(ch);
        i += 1;
    }

    out
}

pub fn complete(prefix: &str, symbols: &[String]) -> Vec<String> {
    let mut candidates = KEYWORDS
        .iter()
        .map(|keyword| keyword.to_string())
        .collect::<Vec<_>>();
    candidates.extend(symbols.iter().cloned());
    candidates.sort();
    candidates.dedup();
    candidates
        .into_iter()
        .filter(|candidate| candidate.starts_with(prefix))
        .collect()
}
