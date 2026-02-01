-- ***************************************************************************
--             Erlang Lexer - Token definitions
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Erlang_Lexer is

   -- Token types for Erlang
   type Token_Type is (
      -- Literals
      TOK_INTEGER,
      TOK_FLOAT,
      TOK_ATOM,
      TOK_VARIABLE,
      TOK_STRING,

      -- Operators
      TOK_PLUS,           -- +
      TOK_MINUS,          -- -
      TOK_MULTIPLY,       -- *
      TOK_DIVIDE,         -- /
      TOK_MOD,            -- mod
      TOK_DIV,            -- div

      -- Comparison
      TOK_EQ,             -- ==
      TOK_NEQ,            -- /=
      TOK_LT,             -- <
      TOK_GT,             -- >
      TOK_LTE,            -- =<
      TOK_GTE,            -- >=
      TOK_EXACT_EQ,       -- =:=
      TOK_EXACT_NEQ,      -- =/=

      -- Assignment and pattern
      TOK_MATCH,          -- =
      TOK_ARROW,          -- ->
      TOK_WHEN,           -- when

      -- Delimiters
      TOK_LPAREN,         -- (
      TOK_RPAREN,         -- )
      TOK_LBRACKET,       -- [
      TOK_RBRACKET,       -- ]
      TOK_LBRACE,         -- {
      TOK_RBRACE,         -- }
      TOK_COMMA,          -- ,
      TOK_SEMICOLON,      -- ;
      TOK_DOT,            -- .
      TOK_PIPE,           -- |
      TOK_COLON,          -- :

      -- Keywords
      TOK_CASE,
      TOK_OF,
      TOK_END,
      TOK_IF,
      TOK_FUN,
      TOK_AFTER,

      -- Special
      TOK_EOF,
      TOK_ERROR
   );

   type Token is record
      TType      : Token_Type;
      Lexeme     : Unbounded_String;
      Line       : Positive := 1;
      Column     : Positive := 1;
      Int_Value  : Integer := 0;
      Float_Value : Float := 0.0;
   end record;

   package Token_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Token);

   --
   -- Tokenize input source code into a vector of tokens
   --
   function Tokenize (Source : String) return Token_Vectors.Vector;

   --
   -- Convert token to string for debugging
   --
   function Token_To_String (T : Token) return String;

private

   --
   -- Check if character is whitespace
   --
   function Is_Whitespace (C : Character) return Boolean;

   --
   -- Check if character can start an atom (lowercase letter)
   --
   function Is_Atom_Start (C : Character) return Boolean;

   --
   -- Check if character can start a variable (uppercase or _)
   --
   function Is_Variable_Start (C : Character) return Boolean;

   --
   -- Check if character is a digit
   --
   function Is_Digit (C : Character) return Boolean;

   --
   -- Check if character is alphanumeric or underscore
   --
   function Is_Alnum (C : Character) return Boolean;

end Erlang_Lexer;
