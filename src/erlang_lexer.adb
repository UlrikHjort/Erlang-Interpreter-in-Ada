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

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Erlang_Lexer is

   --
   -- Check if character is whitespace
   --
   function Is_Whitespace (C : Character) return Boolean is
   begin
      return C = ' ' or C = ASCII.HT or C = ASCII.CR or C = ASCII.LF;
   end Is_Whitespace;

   --
   -- Check if character can start an atom (lowercase letter)
   --
   function Is_Atom_Start (C : Character) return Boolean is
   begin
      return Is_Lower (C);
   end Is_Atom_Start;

   --
   -- Check if character can start a variable (uppercase or _)
   --
   function Is_Variable_Start (C : Character) return Boolean is
   begin
      return Is_Upper (C) or C = '_';
   end Is_Variable_Start;

   --
   -- Check if character is a digit
   --
   function Is_Digit (C : Character) return Boolean is
   begin
      return C >= '0' and C <= '9';
   end Is_Digit;

   --
   -- Check if character is alphanumeric or underscore
   --
   function Is_Alnum (C : Character) return Boolean is
   begin
      return Is_Alphanumeric (C) or C = '_' or C = '@';
   end Is_Alnum;

   --
   -- Tokenize input source code into a vector of tokens
   --
   function Tokenize (Source : String) return Token_Vectors.Vector is
      Tokens : Token_Vectors.Vector;
      Pos    : Positive := Source'First;
      Line   : Positive := 1;
      Col    : Positive := 1;

      --
      -- Peek at current character without advancing
      --
      function Peek return Character is
      begin
         if Pos <= Source'Last then
            return Source (Pos);
         else
            return ASCII.NUL;
         end if;
      end Peek;

      --
      -- Advance position and return current character
      --
      function Advance return Character is
         C : constant Character := Peek;
      begin
         if Pos <= Source'Last then
            Pos := Pos + 1;
            if C = ASCII.LF then
               Line := Line + 1;
               Col := 1;
            else
               Col := Col + 1;
            end if;
         end if;
         return C;
      end Advance;

      --
      -- Add a token to the vector
      --
      procedure Add_Token (TType : Token_Type; Lexeme : String := "") is
         T : Token;
      begin
         T.TType := TType;
         T.Lexeme := To_Unbounded_String (Lexeme);
         T.Line := Line;
         T.Column := Col;
         Tokens.Append (T);
      end Add_Token;

      --
      -- Scan a number (integer or float)
      --
      procedure Scan_Number is
         Lex   : Unbounded_String;
         Is_Float : Boolean := False;
         T : Token;
      begin
         while Is_Digit (Peek) loop
            Lex := Lex & Advance;
         end loop;

         if Peek = '.' and then Pos + 1 <= Source'Last and then Is_Digit (Source (Pos + 1)) then
            Is_Float := True;
            Lex := Lex & Advance;  -- consume '.'
            while Is_Digit (Peek) loop
               Lex := Lex & Advance;
            end loop;
         end if;

         T.Lexeme := Lex;
         T.Line := Line;
         T.Column := Col;

         if Is_Float then
            T.TType := TOK_FLOAT;
            T.Float_Value := Float'Value (To_String (Lex));
         else
            T.TType := TOK_INTEGER;
            T.Int_Value := Integer'Value (To_String (Lex));
         end if;

         Tokens.Append (T);
      end Scan_Number;

      --
      -- Scan an atom or keyword
      --
      procedure Scan_Atom is
         Lex : Unbounded_String;
         T : Token;
      begin
         while Is_Alnum (Peek) loop
            Lex := Lex & Advance;
         end loop;

         T.Lexeme := Lex;
         T.Line := Line;
         T.Column := Col;

         -- Check for keywords
         declare
            Str : constant String := To_String (Lex);
         begin
            if Str = "case" then
               T.TType := TOK_CASE;
            elsif Str = "of" then
               T.TType := TOK_OF;
            elsif Str = "end" then
               T.TType := TOK_END;
            elsif Str = "if" then
               T.TType := TOK_IF;
            elsif Str = "when" then
               T.TType := TOK_WHEN;
            elsif Str = "fun" then
               T.TType := TOK_FUN;
            elsif Str = "after" then
               T.TType := TOK_AFTER;
            elsif Str = "div" then
               T.TType := TOK_DIV;
            elsif Str = "mod" then
               T.TType := TOK_MOD;
            else
               T.TType := TOK_ATOM;
            end if;
         end;

         Tokens.Append (T);
      end Scan_Atom;

      --
      -- Scan a variable
      --
      procedure Scan_Variable is
         Lex : Unbounded_String;
         T : Token;
      begin
         while Is_Alnum (Peek) loop
            Lex := Lex & Advance;
         end loop;

         T.TType := TOK_VARIABLE;
         T.Lexeme := Lex;
         T.Line := Line;
         T.Column := Col;
         Tokens.Append (T);
      end Scan_Variable;

      --
      -- Scan a string literal
      --
      procedure Scan_String is
         Lex : Unbounded_String;
         T : Token;
         Dummy : Character;
      begin
         Dummy := Advance;  -- consume opening "

         while Peek /= '"' and Peek /= ASCII.NUL loop
            if Peek = '\' then
               Dummy := Advance;  -- consume backslash
               case Peek is
                  when 'n' => Lex := Lex & ASCII.LF; Dummy := Advance;
                  when 't' => Lex := Lex & ASCII.HT; Dummy := Advance;
                  when 'r' => Lex := Lex & ASCII.CR; Dummy := Advance;
                  when '\' => Lex := Lex & '\'; Dummy := Advance;
                  when '"' => Lex := Lex & '"'; Dummy := Advance;
                  when others => Lex := Lex & Advance;
               end case;
            else
               Lex := Lex & Advance;
            end if;
         end loop;

         if Peek = '"' then
            Dummy := Advance;  -- consume closing "
         end if;

         T.TType := TOK_STRING;
         T.Lexeme := Lex;
         T.Line := Line;
         T.Column := Col;
         Tokens.Append (T);
      end Scan_String;

      C : Character;
      Dummy : Character;
   begin
      while Pos <= Source'Last loop
         C := Peek;

         -- Skip whitespace
         if Is_Whitespace (C) then
            Dummy := Advance;

         -- Skip comments
         elsif C = '%' then
            while Peek /= ASCII.LF and Peek /= ASCII.NUL loop
               Dummy := Advance;
            end loop;

         -- Numbers
         elsif Is_Digit (C) then
            Scan_Number;

         -- Atoms and keywords
         elsif Is_Atom_Start (C) then
            Scan_Atom;

         -- Variables
         elsif Is_Variable_Start (C) then
            Scan_Variable;

         -- Strings
         elsif C = '"' then
            Scan_String;

         -- Two-character operators
         elsif C = '=' then
            Dummy := Advance;
            if Peek = '=' then
               Dummy := Advance;
               Add_Token (TOK_EQ, "==");
            elsif Peek = ':' and then Pos + 1 <= Source'Last and then Source (Pos + 1) = '=' then
               Dummy := Advance; Dummy := Advance;
               Add_Token (TOK_EXACT_EQ, "=:=");
            elsif Peek = '/' and then Pos + 1 <= Source'Last and then Source (Pos + 1) = '=' then
               Dummy := Advance; Dummy := Advance;
               Add_Token (TOK_EXACT_NEQ, "=/=");
            elsif Peek = '<' then
               Dummy := Advance;
               Add_Token (TOK_LTE, "=<");
            else
               Add_Token (TOK_MATCH, "=");
            end if;

         elsif C = '/' then
            Dummy := Advance;
            if Peek = '=' then
               Dummy := Advance;
               Add_Token (TOK_NEQ, "/=");
            else
               Add_Token (TOK_DIVIDE, "/");
            end if;

         elsif C = '-' then
            Dummy := Advance;
            if Peek = '>' then
               Dummy := Advance;
               Add_Token (TOK_ARROW, "->");
            else
               Add_Token (TOK_MINUS, "-");
            end if;

         elsif C = '>' then
            Dummy := Advance;
            if Peek = '=' then
               Dummy := Advance;
               Add_Token (TOK_GTE, ">=");
            else
               Add_Token (TOK_GT, ">");
            end if;

         elsif C = '<' then
            Dummy := Advance;
            Add_Token (TOK_LT, "<");

         -- Single-character tokens
         elsif C = '+' then
            Dummy := Advance;
            Add_Token (TOK_PLUS, "+");

         elsif C = '*' then
            Dummy := Advance;
            Add_Token (TOK_MULTIPLY, "*");

         elsif C = '(' then
            Dummy := Advance;
            Add_Token (TOK_LPAREN, "(");

         elsif C = ')' then
            Dummy := Advance;
            Add_Token (TOK_RPAREN, ")");

         elsif C = '[' then
            Dummy := Advance;
            Add_Token (TOK_LBRACKET, "[");

         elsif C = ']' then
            Dummy := Advance;
            Add_Token (TOK_RBRACKET, "]");

         elsif C = '{' then
            Dummy := Advance;
            Add_Token (TOK_LBRACE, "{");

         elsif C = '}' then
            Dummy := Advance;
            Add_Token (TOK_RBRACE, "}");

         elsif C = ',' then
            Dummy := Advance;
            Add_Token (TOK_COMMA, ",");

         elsif C = ';' then
            Dummy := Advance;
            Add_Token (TOK_SEMICOLON, ";");

         elsif C = '.' then
            Dummy := Advance;
            Add_Token (TOK_DOT, ".");

         elsif C = '|' then
            Dummy := Advance;
            Add_Token (TOK_PIPE, "|");

         elsif C = ':' then
            Dummy := Advance;
            Add_Token (TOK_COLON, ":");

         else
            -- Unknown character
            Dummy := Advance;
            Add_Token (TOK_ERROR, C & "");
         end if;
      end loop;

      -- Add EOF token
      Add_Token (TOK_EOF, "");

      return Tokens;
   end Tokenize;

   --
   -- Convert token to string for debugging
   --
   function Token_To_String (T : Token) return String is
   begin
      return Token_Type'Image (T.TType) & " '" & To_String (T.Lexeme) & "' at " &
             Positive'Image (T.Line) & ":" & Positive'Image (T.Column);
   end Token_To_String;

end Erlang_Lexer;
