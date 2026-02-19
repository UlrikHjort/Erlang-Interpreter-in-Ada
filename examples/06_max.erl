% Maximum of Two Numbers - Guards with comparison
max(X, Y) when X > Y -> X; max(X, Y) when X =< Y -> Y.
max(42, 17)
