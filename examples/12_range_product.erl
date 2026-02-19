% Range Product - Arithmetic recursion (7! = 5040)
range_product(1) -> 1; range_product(N) when N > 1 -> N * range_product(N - 1).
range_product(7)
