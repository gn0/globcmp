use globcmp_lib::Pattern;
use pyo3::prelude::*;
use std::str::FromStr;

/// Decide whether `a` is weakly more specific than `b`.
///
/// If `a` and `b` are not comparable, then this function returns
/// `False` both ways.  If `a` and `b` are the same, then it returns
/// `True` both ways.
#[pyfunction]
fn is_more_specific_than(a: &str, b: &str) -> bool {
    let pattern_a = Pattern::from_str(a)
        .expect("first argument should be valid glob pattern");
    let pattern_b = Pattern::from_str(b)
        .expect("second argument should be valid glob pattern");

    pattern_a.is_more_specific_than(&pattern_b)
}

/// Count chars in `path` that `pattern` matches without `*` or `**/`.
///
/// Said differently, this function counts the number of chars in `path`
/// that are either
///
/// 1. matched exactly (e.g., `a`),
/// 2. matched by a character class (e.g., `[a-f]`), or
/// 3. matched by a single-character wildcard (`?`).
///
/// Returns `None` if the pattern does not match `path`.
#[pyfunction]
fn count_matching_chars(pattern: &str, path: &str) -> Option<usize> {
    let pattern = Pattern::from_str(pattern)
        .expect("first argument should be valid glob pattern");

    pattern.count_matching_chars(path)
}

#[pymodule]
fn pyglobcmp(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(is_more_specific_than, m)?)?;
    m.add_function(wrap_pyfunction!(count_matching_chars, m)?)?;

    Ok(())
}
