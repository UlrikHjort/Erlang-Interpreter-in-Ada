% Sum of List Elements - List pattern matching, cons [H|T]
sum_list([]) -> 0; sum_list([H | T]) -> H + sum_list(T).
sum_list([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
