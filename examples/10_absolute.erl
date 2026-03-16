% Absolute Value - Negative number handling, guards
abs(N) when N < 0 -> 0 - N; abs(N) when N >= 0 -> N.
abs(0 - 42)
