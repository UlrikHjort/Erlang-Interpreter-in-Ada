-- ***************************************************************************
--              Erlang AST - Abstract Syntax Tree
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

package Erlang_AST is

   -- AST node types
   type AST_Node_Type is (
      AST_Integer,
      AST_Float,
      AST_Atom,
      AST_String,
      AST_Variable,
      AST_Tuple,
      AST_List,
      AST_Cons,           -- List constructor [Head | Tail]
      AST_Binary_Op,
      AST_Unary_Op,
      AST_Function_Call,
      AST_Function_Def,
      AST_Clause,
      AST_Match,
      AST_Case,
      AST_If,
      AST_Block
   );

   type AST_Node;
   type AST_Ptr is access AST_Node;

   package AST_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => AST_Ptr);

   -- Binary operators
   type Binary_Operator is (
      OP_Add, OP_Sub, OP_Mul, OP_Div, OP_Mod, OP_Divide,
      OP_Eq, OP_Neq, OP_Lt, OP_Gt, OP_Lte, OP_Gte,
      OP_Exact_Eq, OP_Exact_Neq
   );

   -- Unary operators
   type Unary_Operator is (
      OP_Neg, OP_Not
   );

   -- Function clause (pattern -> body when guard)
   type Function_Clause is record
      Patterns : AST_Vectors.Vector;
      Guard    : AST_Ptr;
      Clause_Body : AST_Ptr;
   end record;

   type Clause_Ptr is access Function_Clause;

   package Clause_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Clause_Ptr);

   -- Main AST node discriminated record
   type AST_Node (Node_Type : AST_Node_Type := AST_Integer) is record
      case Node_Type is
         when AST_Integer =>
            Int_Value : Integer;

         when AST_Float =>
            Float_Value : Float;

         when AST_Atom =>
            Atom_Name : Unbounded_String;

         when AST_String =>
            Str_Value : Unbounded_String;

         when AST_Variable =>
            Var_Name : Unbounded_String;

         when AST_Tuple =>
            Tuple_Elements : AST_Vectors.Vector;

         when AST_List =>
            List_Elements : AST_Vectors.Vector;

         when AST_Cons =>
            Cons_Head : AST_Ptr;
            Cons_Tail : AST_Ptr;

         when AST_Binary_Op =>
            Bin_Op    : Binary_Operator;
            Bin_Left  : AST_Ptr;
            Bin_Right : AST_Ptr;

         when AST_Unary_Op =>
            Un_Op   : Unary_Operator;
            Un_Expr : AST_Ptr;

         when AST_Function_Call =>
            Call_Name : Unbounded_String;
            Call_Args : AST_Vectors.Vector;

         when AST_Function_Def =>
            Func_Name    : Unbounded_String;
            Func_Clauses : Clause_Vectors.Vector;

         when AST_Clause =>
            Clause_Patterns : AST_Vectors.Vector;
            Clause_Guard    : AST_Ptr;
            Clause_Body     : AST_Ptr;

         when AST_Match =>
            Match_Pattern : AST_Ptr;
            Match_Expr    : AST_Ptr;

         when AST_Case =>
            Case_Expr    : AST_Ptr;
            Case_Clauses : Clause_Vectors.Vector;

         when AST_If =>
            If_Clauses : Clause_Vectors.Vector;

         when AST_Block =>
            Block_Exprs : AST_Vectors.Vector;
      end case;
   end record;

   --
   -- Create integer literal node
   --
   function Make_Integer (Value : Integer) return AST_Ptr;

   --
   -- Create float literal node
   --
   function Make_Float (Value : Float) return AST_Ptr;

   --
   -- Create atom node
   --
   function Make_Atom (Name : String) return AST_Ptr;

   --
   -- Create string node
   --
   function Make_String (Value : String) return AST_Ptr;

   --
   -- Create variable node
   --
   function Make_Variable (Name : String) return AST_Ptr;

   --
   -- Create tuple node
   --
   function Make_Tuple (Elements : AST_Vectors.Vector) return AST_Ptr;

   --
   -- Create list node
   --
   function Make_List (Elements : AST_Vectors.Vector) return AST_Ptr;

   --
   -- Create cons node (list with head and tail)
   --
   function Make_Cons (Head : AST_Ptr; Tail : AST_Ptr) return AST_Ptr;

   --
   -- Create binary operation node
   --
   function Make_Binary_Op (Op : Binary_Operator; Left : AST_Ptr; Right : AST_Ptr) return AST_Ptr;

   --
   -- Create function call node
   --
   function Make_Function_Call (Name : String; Args : AST_Vectors.Vector) return AST_Ptr;

   --
   -- Create match expression node
   --
   function Make_Match (Pattern : AST_Ptr; Expr : AST_Ptr) return AST_Ptr;

   --
   -- Convert AST to string for debugging
   --
   function AST_To_String (Node : AST_Ptr; Indent : Natural := 0) return String;

end Erlang_AST;
