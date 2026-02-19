% Combined Case/If - Classify numbers with case and if guards
classify(N) -> case N of 0 -> zero; X -> if X > 0 -> positive; true -> negative end end.
classify(100)
