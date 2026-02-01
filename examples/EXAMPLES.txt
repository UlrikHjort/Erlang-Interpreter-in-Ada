ERLANG INTERPRETER - EXAMPLE PROGRAMS
======================================

Quick Demo:
-----------
$ make examples

Expected Output:
----------------
01_factorial:         120
02_fibonacci:         55
03_sum_list:          55
04_list_length:       10
05_reverse_list:     [ 5,  4,  3,  2,  1]
06_max:               42
07_power:             1024
08_gcd:               6
09_is_even:          true
10_absolute:          42
11_tuple_access:      123
12_range_product:     5040

What These Demonstrate:
-----------------------
✓ Pattern matching (factorial, tuple_access)
✓ Guards with when clauses (factorial, is_even, max)
✓ Recursion (all examples)
✓ List operations with cons [H|T] (sum_list, reverse_list)
✓ Wildcard patterns _ (is_even, tuple_access)
✓ Mathematical algorithms (gcd, power, fibonacci)
✓ Boolean logic (is_even)
✓ Accumulator patterns (reverse_list)

Run Individual:
---------------
$ ./bin/erlang_interpreter < examples/01_factorial.erl

