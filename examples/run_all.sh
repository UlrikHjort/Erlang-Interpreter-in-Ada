#!/bin/bash
# Run all example programs

INTERPRETER="./bin/erlang_interpreter"
EXAMPLES_DIR="./examples"

echo "======================================"
echo "  Erlang Interpreter - Examples Demo"
echo "======================================"
echo ""

# Check if interpreter exists
if [ ! -f "$INTERPRETER" ]; then
    echo "Error: Interpreter not found. Please run 'make' first."
    exit 1
fi

# Run each example
for example in "$EXAMPLES_DIR"/*.erl; do
    if [ -f "$example" ]; then
        filename=$(basename "$example" .erl)
        printf "%-20s " "$filename:"
        
        # Run the example - it already calls the function
        output=$("$INTERPRETER" < "$example" 2>&1 | grep "=> " | tail -1)
        
        # Extract just the value part (remove "> => " prefix)
        result=$(echo "$output" | sed 's/^> => //')
        echo "$result"
    fi
done

echo ""
echo "======================================"
echo "  All examples completed!"
echo "======================================"
