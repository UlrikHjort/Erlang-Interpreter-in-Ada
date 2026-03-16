% Case Expression - Pattern matching with case/of/end
sign(X) -> case X of 0 -> zero; N when N > 0 -> positive; _ -> negative end.
sign(42)
