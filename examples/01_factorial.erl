% Factorial Example - Pattern matching, guards, recursion
factorial(0) -> 1; factorial(N) when N > 0 -> N * factorial(N - 1).
factorial(5)
