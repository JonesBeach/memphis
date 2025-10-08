const EPSILON: f64 = 1e-9;

pub fn floats_equal(a: f64, b: f64) -> bool {
    (a - b).abs() < EPSILON
}
