% Working examples for REPL (single line per construct)
% Factorial - single line with semicolons
factorial(0) -> 1; factorial(N) when N > 0 -> N * factorial(N - 1).

% Simple test function
simple_test() -> X = 42.

% Pattern matching examples
first({X, _, _}) -> X.
second({_, Y, _}) -> Y.

% List operations
sum_list([]) -> 0; sum_list([H | T]) -> H + sum_list(T).
