% Greatest Common Divisor - Euclidean algorithm with modulo
gcd(A, 0) -> A; gcd(A, B) -> gcd(B, A mod B).
gcd(48, 18)
