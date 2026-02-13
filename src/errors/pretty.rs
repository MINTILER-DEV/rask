pub fn underline(line: &str, column: usize) -> String {
    let mut marker = String::new();
    for _ in 1..column {
        marker.push(' ');
    }
    marker.push('^');
    format!("{}\n{}", line, marker)
}
