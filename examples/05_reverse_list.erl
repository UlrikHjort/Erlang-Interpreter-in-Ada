% Reverse a List - Accumulator pattern, helper functions
reverse_helper([], Acc) -> Acc; reverse_helper([H | T], Acc) -> reverse_helper(T, [H | Acc]).
reverse(List) -> reverse_helper(List, []).
reverse([1, 2, 3, 4, 5])
