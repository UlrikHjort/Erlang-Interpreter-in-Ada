% Check if Number is Even - Modulo operator, boolean atoms
is_even(N) when N mod 2 == 0 -> true; is_even(N) when N mod 2 /= 0 -> false.
is_even(42)
