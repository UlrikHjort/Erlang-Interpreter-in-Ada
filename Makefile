# Makefile for Erlang Interpreter in Ada
# Copyright (C) 2026 By Ulrik Hørlyk Hjort

# Compiler and flags
GNATMAKE = gnatmake
GNATCLEAN = gnatclean
ADAFLAGS = -I./src -D ./obj -gnat2012 -gnata -gnatwa -gnatwe -O2

# Directories
SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin
TEST_DIR = tests

# Main executable
MAIN = erlang_interpreter
MAIN_SRC = $(SRC_DIR)/main.adb

# All targets
.PHONY: all clean test run help examples

all: $(BIN_DIR)/$(MAIN)

$(BIN_DIR)/$(MAIN): $(MAIN_SRC)
	@mkdir -p $(BIN_DIR)
	@mkdir -p $(OBJ_DIR)
	$(GNATMAKE) $(ADAFLAGS) -o $@ $<

clean:
	$(GNATCLEAN) -r -D $(OBJ_DIR)
	rm -rf $(BIN_DIR)/* $(OBJ_DIR)/*

run: $(BIN_DIR)/$(MAIN)
	@$(BIN_DIR)/$(MAIN)

test: $(BIN_DIR)/$(MAIN)
	@echo "Running tests..."
	@cd $(TEST_DIR) && ../$(BIN_DIR)/$(MAIN) test_suite.erl

examples: $(BIN_DIR)/$(MAIN)
	@./examples/run_all.sh

help:
	@echo "Erlang Interpreter - Build System"
	@echo "=================================="
	@echo "Targets:"
	@echo "  all      - Build the interpreter (default)"
	@echo "  clean    - Remove build artifacts"
	@echo "  run      - Build and run the interpreter"
	@echo "  test     - Run test suite"
	@echo "  examples - Run all example programs"
	@echo "  help     - Show this help message"
