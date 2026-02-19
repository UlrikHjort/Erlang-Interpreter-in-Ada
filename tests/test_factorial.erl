% Corrected Erlang test file - REPL compatible format
% All function clauses must be on single line

% Factorial with pattern matching and guard
factorial(0) -> 1; factorial(N) when N > 0 -> N * factorial(N - 1).

% Simple test function (single expression)
test() -> {atom, 123, "hello"}.

% Pattern matching on tuples
first({X, _, _}) -> X.

% List operations
length_list([]) -> 0; length_list([_ | T]) -> 1 + length_list(T).
sum_list([]) -> 0; sum_list([H | T]) -> H + sum_list(T).

