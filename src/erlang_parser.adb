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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Erlang_Parser is

   --
   -- Get current token
   --
   function Current (Tokens : Token_Vectors.Vector; Pos : Positive) return Token is
   begin
      if Pos <= Positive (Tokens.Length) then
         return Tokens.Element (Pos);
      else
         return Token'(TType => TOK_EOF, Lexeme => Null_Unbounded_String,
                      Line => 1, Column => 1, Int_Value => 0, Float_Value => 0.0);
      end if;
   end Current;

   --
   -- Advance to next token
   --
   procedure Advance (Pos : in out Positive) is
   begin
      Pos := Pos + 1;
   end Advance;

   --
   -- Check if current token matches expected type
   --
   function Match (Tokens : Token_Vectors.Vector; Pos : Positive; TType : Token_Type) return Boolean is
   begin
      return Current (Tokens, Pos).TType = TType;
   end Match;

   --
   -- Expect and consume a specific token type
   --
   procedure Expect (Tokens : Token_Vectors.Vector; Pos : in out Positive; TType : Token_Type) is
   begin
      if not Match (Tokens, Pos, TType) then
         raise Parse_Error with "Expected " & Token_Type'Image (TType) &
               " but got " & Token_Type'Image (Current (Tokens, Pos).TType);
      end if;
      Advance (Pos);
   end Expect;

   --
   -- Parse primary expression (literals, variables, tuples, lists, parenthesized)
   --
   function Parse_Primary (Tokens : Token_Vectors.Vector; Pos : in out Positive) return AST_Ptr is
      Tok : constant Token := Current (Tokens, Pos);
   begin
      case Tok.TType is
         when TOK_INTEGER =>
            Advance (Pos);
            return Make_Integer (Tok.Int_Value);

         when TOK_FLOAT =>
            Advance (Pos);
            return Make_Float (Tok.Float_Value);

         when TOK_STRING =>
            Advance (Pos);
            return Make_String (To_String (Tok.Lexeme));

         when TOK_VARIABLE =>
            Advance (Pos);
            return Make_Variable (To_String (Tok.Lexeme));

         when TOK_ATOM =>
            declare
               Name : constant String := To_String (Tok.Lexeme);
            begin
               Advance (Pos);

               -- Check for function call
               if Match (Tokens, Pos, TOK_LPAREN) then
                  Advance (Pos);  -- consume '('

                  declare
                     Args : AST_Vectors.Vector;
                  begin
                     -- Parse arguments
                     if not Match (Tokens, Pos, TOK_RPAREN) then
                        loop
                           Args.Append (Parse_Expression (Tokens, Pos));
                           exit when not Match (Tokens, Pos, TOK_COMMA);
                           Advance (Pos);  -- consume ','
                        end loop;
                     end if;

                     Expect (Tokens, Pos, TOK_RPAREN);
                     return Make_Function_Call (Name, Args);
                  end;
               else
                  -- Just an atom
                  return Make_Atom (Name);
               end if;
            end;

         when TOK_LBRACE =>
            -- Tuple
            Advance (Pos);  -- consume '{'
            declare
               Elements : AST_Vectors.Vector;
            begin
               if not Match (Tokens, Pos, TOK_RBRACE) then
                  loop
                     Elements.Append (Parse_Expression (Tokens, Pos));
                     exit when not Match (Tokens, Pos, TOK_COMMA);
                     Advance (Pos);  -- consume ','
                  end loop;
               end if;

               Expect (Tokens, Pos, TOK_RBRACE);
               return Make_Tuple (Elements);
            end;

         when TOK_LBRACKET =>
            -- List
            Advance (Pos);  -- consume '['
            declare
               Elements : AST_Vectors.Vector;
            begin
               if not Match (Tokens, Pos, TOK_RBRACKET) then
                  loop
                     Elements.Append (Parse_Expression (Tokens, Pos));

                     if Match (Tokens, Pos, TOK_PIPE) then
                        -- List with tail [H1, H2 | Tail]
                        Advance (Pos);  -- consume '|'
                        declare
                           Tail : constant AST_Ptr := Parse_Expression (Tokens, Pos);
                           Result : AST_Ptr := Tail;
                        begin
                           -- Build cons cells from right to left
                           for I in reverse 1 .. Natural (Elements.Length) loop
                              Result := Make_Cons (Elements.Element (I), Result);
                           end loop;
                           Expect (Tokens, Pos, TOK_RBRACKET);
                           return Result;
                        end;
                     end if;

                     exit when not Match (Tokens, Pos, TOK_COMMA);
                     Advance (Pos);  -- consume ','
                  end loop;
               end if;

               Expect (Tokens, Pos, TOK_RBRACKET);
               return Make_List (Elements);
            end;

         when TOK_LPAREN =>
            -- Parenthesized expression
            Advance (Pos);  -- consume '('
            declare
               Expr : constant AST_Ptr := Parse_Expression (Tokens, Pos);
            begin
               Expect (Tokens, Pos, TOK_RPAREN);
               return Expr;
            end;

         when TOK_CASE =>
            -- case Expr of Pattern1 -> Body1; Pattern2 -> Body2 end
            Advance (Pos);  -- consume 'case'
            declare
               Expr : constant AST_Ptr := Parse_Expression (Tokens, Pos);
               Clauses : Clause_Vectors.Vector;
            begin
               Expect (Tokens, Pos, TOK_OF);

               -- Parse case clauses
               loop
                  declare
                     Pattern : constant AST_Ptr := Parse_Expression (Tokens, Pos);
                     Guard   : AST_Ptr := null;
                     Clause_Body    : AST_Ptr;
                  begin
                     -- Optional when guard
                     if Match (Tokens, Pos, TOK_WHEN) then
                        Advance (Pos);
                        Guard := Parse_Expression (Tokens, Pos);
                     end if;

                     Expect (Tokens, Pos, TOK_ARROW);
                     Clause_Body := Parse_Expression (Tokens, Pos);

                     declare
                        Clause : constant Clause_Ptr := new Function_Clause;
                     begin
                        Clause.Patterns.Append (Pattern);
                        Clause.Guard := Guard;
                        Clause.Clause_Body := Clause_Body;
                        Clauses.Append (Clause);
                     end;
                  end;

                  exit when not Match (Tokens, Pos, TOK_SEMICOLON);
                  Advance (Pos);  -- consume ';'
               end loop;

               Expect (Tokens, Pos, TOK_END);
               return new AST_Node'(Node_Type => AST_Case, Case_Expr => Expr, Case_Clauses => Clauses);
            end;

         when TOK_IF =>
            -- if Guard1 -> Body1; Guard2 -> Body2 end
            Advance (Pos);  -- consume 'if'
            declare
               Clauses : Clause_Vectors.Vector;
            begin
               -- Parse if clauses
               loop
                  declare
                     Guard : constant AST_Ptr := Parse_Expression (Tokens, Pos);
                     Clause_Body  : AST_Ptr;
                  begin
                     Expect (Tokens, Pos, TOK_ARROW);
                     Clause_Body := Parse_Expression (Tokens, Pos);

                     declare
                        Clause : constant Clause_Ptr := new Function_Clause;
                        Empty_Patterns : AST_Vectors.Vector;
                     begin
                        Clause.Patterns := Empty_Patterns;
                        Clause.Guard := Guard;
                        Clause.Clause_Body := Clause_Body;
                        Clauses.Append (Clause);
                     end;
                  end;

                  exit when not Match (Tokens, Pos, TOK_SEMICOLON);
                  Advance (Pos);  -- consume ';'
               end loop;

               Expect (Tokens, Pos, TOK_END);
               return new AST_Node'(Node_Type => AST_If, If_Clauses => Clauses);
            end;

         when others =>
            raise Parse_Error with "Unexpected token: " & Token_Type'Image (Tok.TType);
      end case;
   end Parse_Primary;

   --
   -- Parse multiplicative expression (* / div mod)
   --
   function Parse_Multiplicative (Tokens : Token_Vectors.Vector; Pos : in out Positive) return AST_Ptr is
      Left : AST_Ptr := Parse_Primary (Tokens, Pos);
   begin
      loop
         if Match (Tokens, Pos, TOK_MULTIPLY) then
            Advance (Pos);
            Left := Make_Binary_Op (OP_Mul, Left, Parse_Primary (Tokens, Pos));
         elsif Match (Tokens, Pos, TOK_DIVIDE) then
            Advance (Pos);
            Left := Make_Binary_Op (OP_Divide, Left, Parse_Primary (Tokens, Pos));
         elsif Match (Tokens, Pos, TOK_DIV) then
            Advance (Pos);
            Left := Make_Binary_Op (OP_Div, Left, Parse_Primary (Tokens, Pos));
         elsif Match (Tokens, Pos, TOK_MOD) then
            Advance (Pos);
            Left := Make_Binary_Op (OP_Mod, Left, Parse_Primary (Tokens, Pos));
         else
            exit;
         end if;
      end loop;
      return Left;
   end Parse_Multiplicative;

   --
   -- Parse additive expression (+ -)
   --
   function Parse_Additive (Tokens : Token_Vectors.Vector; Pos : in out Positive) return AST_Ptr is
      Left : AST_Ptr := Parse_Multiplicative (Tokens, Pos);
   begin
      loop
         if Match (Tokens, Pos, TOK_PLUS) then
            Advance (Pos);
            Left := Make_Binary_Op (OP_Add, Left, Parse_Multiplicative (Tokens, Pos));
         elsif Match (Tokens, Pos, TOK_MINUS) then
            Advance (Pos);
            Left := Make_Binary_Op (OP_Sub, Left, Parse_Multiplicative (Tokens, Pos));
         else
            exit;
         end if;
      end loop;
      return Left;
   end Parse_Additive;

   --
   -- Parse comparison expression (< > =< >= == /= =:= =/=)
   --
   function Parse_Comparison (Tokens : Token_Vectors.Vector; Pos : in out Positive) return AST_Ptr is
      Left : constant AST_Ptr := Parse_Additive (Tokens, Pos);
   begin
      if Match (Tokens, Pos, TOK_LT) then
         Advance (Pos);
         return Make_Binary_Op (OP_Lt, Left, Parse_Additive (Tokens, Pos));
      elsif Match (Tokens, Pos, TOK_GT) then
         Advance (Pos);
         return Make_Binary_Op (OP_Gt, Left, Parse_Additive (Tokens, Pos));
      elsif Match (Tokens, Pos, TOK_LTE) then
         Advance (Pos);
         return Make_Binary_Op (OP_Lte, Left, Parse_Additive (Tokens, Pos));
      elsif Match (Tokens, Pos, TOK_GTE) then
         Advance (Pos);
         return Make_Binary_Op (OP_Gte, Left, Parse_Additive (Tokens, Pos));
      elsif Match (Tokens, Pos, TOK_EQ) then
         Advance (Pos);
         return Make_Binary_Op (OP_Eq, Left, Parse_Additive (Tokens, Pos));
      elsif Match (Tokens, Pos, TOK_NEQ) then
         Advance (Pos);
         return Make_Binary_Op (OP_Neq, Left, Parse_Additive (Tokens, Pos));
      elsif Match (Tokens, Pos, TOK_EXACT_EQ) then
         Advance (Pos);
         return Make_Binary_Op (OP_Exact_Eq, Left, Parse_Additive (Tokens, Pos));
      elsif Match (Tokens, Pos, TOK_EXACT_NEQ) then
         Advance (Pos);
         return Make_Binary_Op (OP_Exact_Neq, Left, Parse_Additive (Tokens, Pos));
      end if;

      return Left;
   end Parse_Comparison;

   --
   -- Parse match expression (=)
   --
   function Parse_Match (Tokens : Token_Vectors.Vector; Pos : in out Positive) return AST_Ptr is
      Left : constant AST_Ptr := Parse_Comparison (Tokens, Pos);
   begin
      if Match (Tokens, Pos, TOK_MATCH) then
         Advance (Pos);
         return Make_Match (Left, Parse_Comparison (Tokens, Pos));
      end if;
      return Left;
   end Parse_Match;

   --
   -- Parse a single expression
   --
   function Parse_Expression (Tokens : Token_Vectors.Vector; Pos : in out Positive) return AST_Ptr is
   begin
      return Parse_Match (Tokens, Pos);
   end Parse_Expression;

   --
   -- Parse tokens into an AST expression
   --
   function Parse (Tokens : Token_Vectors.Vector) return AST_Ptr is
      Pos : Positive := 1;
      
      --
      -- Check if this looks like a function definition
      -- (has an arrow token somewhere)
      --
      function Has_Arrow return Boolean is
      begin
         for I in 1 .. Natural (Tokens.Length) loop
            if Tokens.Element (I).TType = TOK_ARROW then
               return True;
            end if;
         end loop;
         return False;
      end Has_Arrow;
      
   begin
      if Natural (Tokens.Length) = 0 or else Tokens.Element (1).TType = TOK_EOF then
         return null;
      end if;

      -- Check if it's a function definition (atom followed by lparen and has arrow)
      if Natural (Tokens.Length) >= 2 and then
         Tokens.Element (1).TType = TOK_ATOM and then
         Tokens.Element (2).TType = TOK_LPAREN and then
         Has_Arrow
      then
         return Parse_Function_Definition (Tokens, Pos);
      else
         return Parse_Expression (Tokens, Pos);
      end if;
   end Parse;

   --
   -- Parse a function clause (patterns) -> body
   --
   function Parse_Clause (Tokens : Token_Vectors.Vector; Pos : in out Positive; Func_Name : String) return Clause_Ptr is
      Clause : constant Clause_Ptr := new Function_Clause;
   begin
      -- Expect function name
      if not Match (Tokens, Pos, TOK_ATOM) or else To_String (Current (Tokens, Pos).Lexeme) /= Func_Name then
         raise Parse_Error with "Expected function name " & Func_Name;
      end if;
      Advance (Pos);

      -- Parse patterns
      Expect (Tokens, Pos, TOK_LPAREN);

      if not Match (Tokens, Pos, TOK_RPAREN) then
         loop
            Clause.Patterns.Append (Parse_Expression (Tokens, Pos));
            exit when not Match (Tokens, Pos, TOK_COMMA);
            Advance (Pos);
         end loop;
      end if;

      Expect (Tokens, Pos, TOK_RPAREN);

      -- Optional guard
      if Match (Tokens, Pos, TOK_WHEN) then
         Advance (Pos);
         Clause.Guard := Parse_Expression (Tokens, Pos);
      else
         Clause.Guard := null;
      end if;

      -- Arrow
      Expect (Tokens, Pos, TOK_ARROW);

      -- Body
      Clause.Clause_Body := Parse_Expression (Tokens, Pos);

      return Clause;
   end Parse_Clause;

   --
   -- Parse a function definition (multiple clauses)
   --
   function Parse_Function_Definition (Tokens : Token_Vectors.Vector; Pos : in out Positive) return AST_Ptr is
      Func_Name : constant String := To_String (Current (Tokens, Pos).Lexeme);
      Clauses : Clause_Vectors.Vector;
   begin
      -- Parse first clause
      Clauses.Append (Parse_Clause (Tokens, Pos, Func_Name));

      -- Parse additional clauses (separated by semicolon)
      while Match (Tokens, Pos, TOK_SEMICOLON) loop
         Advance (Pos);
         
         -- Check if next token is the same function name
         if Match (Tokens, Pos, TOK_ATOM) and then To_String (Current (Tokens, Pos).Lexeme) = Func_Name then
            Clauses.Append (Parse_Clause (Tokens, Pos, Func_Name));
         else
            exit;
         end if;
      end loop;

      -- Expect final dot
      if Match (Tokens, Pos, TOK_DOT) then
         Advance (Pos);
      end if;

      return new AST_Node'(Node_Type => AST_Function_Def,
                          Func_Name => To_Unbounded_String (Func_Name),
                          Func_Clauses => Clauses);
   end Parse_Function_Definition;

end Erlang_Parser;
