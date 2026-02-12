% List Length - Wildcard pattern _, list recursion
length_list([]) -> 0; length_list([_ | T]) -> 1 + length_list(T).
length_list([a, b, c, d, e, f, g, h, i, j])
