use globcmp_lib::Pattern;
use pyo3::prelude::*;
use std::str::FromStr;

#[pyfunction]
fn is_more_specific_than(a: &str, b: &str) -> bool {
    let pattern_a = Pattern::from_str(a)
        .expect("first argument should be valid glob pattern");
    let pattern_b = Pattern::from_str(b)
        .expect("second argument should be valid glob pattern");

    pattern_a.is_more_specific_than(&pattern_b)
}

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
