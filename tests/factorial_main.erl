% Complete factorial test with function call
factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

main() -> factorial(5).
