% Tuple Pattern Matching - Destructuring tuples, wildcard
first({X, _, _}) -> X.
second({_, Y, _}) -> Y.
third({_, _, Z}) -> Z.
third({hello, world, 123})
