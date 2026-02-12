# Erlang Interpreter in Ada


<p align="center">
  <img src="https://img.shields.io/badge/Ada-2012-blue.svg" alt="Ada 2012">
   <img src="https://img.shields.io/badge/Erlang-white.svg?style=for-the-badge&logo=erlang&logoColor=a90533" alt="Erlang">
  <img src="https://img.shields.io/badge/license-MIT-green.svg" alt="MIT License">
</p>

A working **[Erlang](https://en.wikipedia.org/wiki/Erlang_(programming_language))** interpreter written in Ada, implementing core language features with pattern matching, guards, and recursive functions.


## Features

### Fully Implemented
- **Lexer**: Complete tokenization of Erlang source code
  - Literals: integers, floats, atoms, strings, variables
  - Operators: arithmetic (+, -, *, /, div, mod), comparison (==, /=, <, >, =<, >=, =:=, =/=)
  - Delimiters: parentheses, brackets, braces, comma, semicolon, dot, pipe, colon
  - Keywords: case, of, end, if, when, fun, after
  - Comments (% style)

- **Parser**: Converts tokens to Abstract Syntax Tree
  - Expression parsing with correct operator precedence
  - Function definitions with multiple clauses
  - Pattern syntax for literals, variables, tuples, lists, cons
  - Case expressions with pattern matching
  - If expressions with guards

- **Evaluator**: Executes AST expressions
  - All arithmetic and comparison operators
  - Variable bindings (persistent in REPL)
  - Tuples: `{atom, 123, "hello"}`
  - Lists: `[1, 2, 3]`
  - List cons: `[Head | Tail]`
  - Case expressions for pattern-based control flow
  - If expressions for guard-based conditionals

- **Pattern Matching**:
  - Literals (integers, floats, atoms, strings)
  - Variables (with binding)
  - Wildcard `_` (matches anything)
  - Tuples: `{X, Y, Z}`
  - Lists: `[H | T]`, `[1, 2, 3]`
  - Complex nested patterns

- **Functions**:
  - Multi-clause definitions
  - Pattern matching on arguments
  - Guards with `when` clauses
  - Recursive calls (factorial, fibonacci, list operations)
  - Proper tail recursion support

- **Built-in Functions**:
  - `length/1` - Get length of list
  - `hd/1` - Get head of list
  - `tl/1` - Get tail of list
  - `is_integer/1`, `is_float/1`, `is_atom/1`, `is_list/1`, `is_tuple/1` - Type checks
  - `print/1`, `println/1` - Output functions

## Building

```bash
make          # Build the interpreter
make clean    # Clean build artifacts
make run      # Build and run REPL
make test     # Run test suite
make examples # Run all example programs
```

## Requirements

- GNAT Ada compiler (Ada 2012 or later)
- GNU Make

## Usage

```bash
# Start REPL
./bin/erlang_interpreter

# Execute Erlang file
./bin/erlang_interpreter < file.erl

# Run tests
./bin/erlang_interpreter --test
```

## Examples

### Variables and Arithmetic
```erlang
> X = 10
=> 10
> Y = X * 2
=> 20
> 2 + 3 * 4
=> 14
```

### Data Structures and Pattern Matching
```erlang
> {atom, 123, "hello"}
=> {atom, 123, "hello"}
> [1, 2, 3 | [4, 5]]
=> [1, 2, 3, 4, 5]

% Direct pattern matching on tuples
> {X, Y, Z} = {10, 20, 30}
=> {10, 20, 30}
> X
=> 10

% List destructuring with cons
> [H | T] = [1, 2, 3, 4]
=> [1, 2, 3, 4]
> H
=> 1
> T
=> [2, 3, 4]

% Nested patterns
> {Name, {X_Coord, Y_Coord}} = {point, {5, 10}}
=> {point, {5, 10}}
> Name
=> point
```

### Functions with Pattern Matching
```erlang
> factorial(0) -> 1; factorial(N) when N > 0 -> N * factorial(N - 1).
=> factorial
> factorial(5)
=> 120

> sum_list([]) -> 0; sum_list([H | T]) -> H + sum_list(T).
=> sum_list
> sum_list([1, 2, 3, 4, 5])
=> 15

> first({X, _, _}) -> X.
=> first
> first({hello, world, 123})
=> hello
```

### Fibonacci
```erlang
> fib(0) -> 0; fib(1) -> 1; fib(N) when N > 1 -> fib(N - 1) + fib(N - 2).
=> fib
> fib(10)
=> 55
```

### Case Expressions
```erlang
> sign(X) -> case X of 0 -> zero; N when N > 0 -> positive; _ -> negative end.
=> sign
> sign(42)
=> positive
> sign(0)
=> zero
```

### If Expressions
```erlang
> max(X, Y) -> if X > Y -> X; true -> Y end.
=> max
> max(10, 20)
=> 20
```

### Built-in Functions
```erlang
> length([1, 2, 3, 4, 5])
=> 5
> hd([10, 20, 30])
=> 10
> tl([10, 20, 30])
=> [20, 30]
> is_list([1, 2, 3])
=> true
> println("Hello Erlang!")
Hello Erlang!
=> ok
```



## Future Enhancements:
- List comprehensions
- Anonymous functions (fun)
- More built-in functions (io:format/2, lists:map, etc.)
- Process spawning and message passing
- Module system
- Error handling (try-catch)

## Technical Highlights

- **Dynamic typing** implemented via Ada variant records
- **Pattern matching** with proper variable binding and unification
- **Tail recursion** supported for efficient recursive functions
- **Operator precedence** correctly handled in parser
- **Persistent environment** in REPL session
- **Clean separation** of lexer → parser → evaluator stages

## Known Limitations

- **REPL processes one line at a time**: Multi-line function definitions must be written on a single line with semicolons separating clauses
- **File mode evaluates first expression only**: Use REPL to call functions after loading definitions


