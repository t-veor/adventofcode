pub fn gcd(a: i64, b: i64) -> i64 {
    egcd(a, b).0
}

pub fn lcm(a: i64, b: i64) -> i64 {
    if a == 0 || b == 0 {
        0
    } else {
        let g = gcd(a, b);
        let coeff = b / g;
        a * coeff
    }
}

pub fn egcd(a: i64, b: i64) -> (i64, i64, i64) {
    if a == 0 {
        (b, 0, 1)
    } else {
        let (g, x, y) = egcd(b % a, a);
        (g, y - (b / a) * x, x)
    }
}

pub fn multiplicative_inverse(x: i64, base: i64) -> Option<i64> {
    let (g, x, _) = egcd(x, base);
    if g == 1 {
        Some(x.rem_euclid(base))
    } else {
        None
    }
}

pub fn chinese_remainder_theorem(residues: &[i64], bases: &[i64]) -> Option<i64> {
    if residues.len() != bases.len() {
        return None;
    }

    let product = bases.iter().product::<i64>();
    let mut sum = 0;

    for (&residue, &base) in residues.iter().zip(bases) {
        let p = product / base;
        sum += residue * multiplicative_inverse(p, base)? * p;
    }

    Some(sum.rem_euclid(product))
}

// TODO: I'm not sure if this implementation is fully correct. At the very least
// it should be cleaned up a bit
pub fn chinese_remainder_theorem_non_coprime(residues: &[i64], bases: &[i64]) -> Option<i64> {
    if residues.len() != bases.len() {
        return None;
    }

    if residues.len() == 0 {
        return None;
    }

    let mut a = residues[0];
    let mut m = bases[0];
    for i in 1..residues.len() {
        let b = residues[i];
        let n = bases[i];

        let (g, u, _) = egcd(m, n);

        let diff = a - b;
        if diff % g != 0 {
            return None;
        }

        let lambda = diff / g;
        let sigma = a - m * u * lambda;

        a = sigma;
        m = m * (n / g);
    }

    Some(a.rem_euclid(m))
}
