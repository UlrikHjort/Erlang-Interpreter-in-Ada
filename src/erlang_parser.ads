-- ***************************************************************************
--           Erlang Parser - Token to AST conversion
--
--           Copyright (C) 2026 By Ulrik Hørlyk Hjort
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-- ***************************************************************************

with Erlang_Lexer; use Erlang_Lexer;
with Erlang_AST; use Erlang_AST;

package Erlang_Parser is

   Parse_Error : exception;

   --
   -- Parse tokens into an AST expression
   --
   function Parse (Tokens : Token_Vectors.Vector) return AST_Ptr;

   --
   -- Parse a single expression from tokens
   --
   function Parse_Expression (Tokens : Token_Vectors.Vector; Pos : in out Positive) return AST_Ptr;

   --
   -- Parse a function definition (multiple clauses)
   --
   function Parse_Function_Definition (Tokens : Token_Vectors.Vector; Pos : in out Positive) return AST_Ptr;

end Erlang_Parser;
