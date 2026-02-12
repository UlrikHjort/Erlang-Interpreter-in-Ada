% Power Function (X^N) - Mathematical recursion
power(_, 0) -> 1; power(X, N) when N > 0 -> X * power(X, N - 1).
power(2, 10)
