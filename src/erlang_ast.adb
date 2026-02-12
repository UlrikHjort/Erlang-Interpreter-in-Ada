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

package body Erlang_AST is

   --
   -- Create integer literal node
   --
   function Make_Integer (Value : Integer) return AST_Ptr is
   begin
      return new AST_Node'(Node_Type => AST_Integer, Int_Value => Value);
   end Make_Integer;

   --
   -- Create float literal node
   --
   function Make_Float (Value : Float) return AST_Ptr is
   begin
      return new AST_Node'(Node_Type => AST_Float, Float_Value => Value);
   end Make_Float;

   --
   -- Create atom node
   --
   function Make_Atom (Name : String) return AST_Ptr is
   begin
      return new AST_Node'(Node_Type => AST_Atom, Atom_Name => To_Unbounded_String (Name));
   end Make_Atom;

   --
   -- Create string node
   --
   function Make_String (Value : String) return AST_Ptr is
   begin
      return new AST_Node'(Node_Type => AST_String, Str_Value => To_Unbounded_String (Value));
   end Make_String;

   --
   -- Create variable node
   --
   function Make_Variable (Name : String) return AST_Ptr is
   begin
      return new AST_Node'(Node_Type => AST_Variable, Var_Name => To_Unbounded_String (Name));
   end Make_Variable;

   --
   -- Create tuple node
   --
   function Make_Tuple (Elements : AST_Vectors.Vector) return AST_Ptr is
   begin
      return new AST_Node'(Node_Type => AST_Tuple, Tuple_Elements => Elements);
   end Make_Tuple;

   --
   -- Create list node
   --
   function Make_List (Elements : AST_Vectors.Vector) return AST_Ptr is
   begin
      return new AST_Node'(Node_Type => AST_List, List_Elements => Elements);
   end Make_List;

   --
   -- Create cons node (list with head and tail)
   --
   function Make_Cons (Head : AST_Ptr; Tail : AST_Ptr) return AST_Ptr is
   begin
      return new AST_Node'(Node_Type => AST_Cons, Cons_Head => Head, Cons_Tail => Tail);
   end Make_Cons;

   --
   -- Create binary operation node
   --
   function Make_Binary_Op (Op : Binary_Operator; Left : AST_Ptr; Right : AST_Ptr) return AST_Ptr is
   begin
      return new AST_Node'(Node_Type => AST_Binary_Op, Bin_Op => Op, Bin_Left => Left, Bin_Right => Right);
   end Make_Binary_Op;

   --
   -- Create function call node
   --
   function Make_Function_Call (Name : String; Args : AST_Vectors.Vector) return AST_Ptr is
   begin
      return new AST_Node'(Node_Type => AST_Function_Call, Call_Name => To_Unbounded_String (Name), Call_Args => Args);
   end Make_Function_Call;

   --
   -- Create match expression node
   --
   function Make_Match (Pattern : AST_Ptr; Expr : AST_Ptr) return AST_Ptr is
   begin
      return new AST_Node'(Node_Type => AST_Match, Match_Pattern => Pattern, Match_Expr => Expr);
   end Make_Match;

   --
   -- Convert AST to string for debugging
   --
   function AST_To_String (Node : AST_Ptr; Indent : Natural := 0) return String is
      Prefix : constant String := (1 .. Indent * 2 => ' ');
   begin
      if Node = null then
         return Prefix & "<null>";
      end if;

      case Node.Node_Type is
         when AST_Integer =>
            return Prefix & "Int(" & Integer'Image (Node.Int_Value) & ")";

         when AST_Float =>
            return Prefix & "Float(" & Float'Image (Node.Float_Value) & ")";

         when AST_Atom =>
            return Prefix & "Atom(" & To_String (Node.Atom_Name) & ")";

         when AST_String =>
            return Prefix & "String(""" & To_String (Node.Str_Value) & """)";

         when AST_Variable =>
            return Prefix & "Var(" & To_String (Node.Var_Name) & ")";

         when AST_Tuple =>
            declare
               Result : Unbounded_String := To_Unbounded_String (Prefix & "Tuple[");
            begin
               for I in 1 .. Natural (Node.Tuple_Elements.Length) loop
                  if I > 1 then
                     Append (Result, ", ");
                  end if;
                  Append (Result, AST_To_String (Node.Tuple_Elements.Element (I), 0));
               end loop;
               Append (Result, "]");
               return To_String (Result);
            end;

         when AST_List =>
            declare
               Result : Unbounded_String := To_Unbounded_String (Prefix & "List[");
            begin
               for I in 1 .. Natural (Node.List_Elements.Length) loop
                  if I > 1 then
                     Append (Result, ", ");
                  end if;
                  Append (Result, AST_To_String (Node.List_Elements.Element (I), 0));
               end loop;
               Append (Result, "]");
               return To_String (Result);
            end;

         when AST_Cons =>
            return Prefix & "Cons[" & AST_To_String (Node.Cons_Head, 0) &
                   " | " & AST_To_String (Node.Cons_Tail, 0) & "]";

         when AST_Binary_Op =>
            return Prefix & "BinOp(" & Binary_Operator'Image (Node.Bin_Op) & ", " &
                   AST_To_String (Node.Bin_Left, 0) & ", " &
                   AST_To_String (Node.Bin_Right, 0) & ")";

         when AST_Function_Call =>
            declare
               Result : Unbounded_String := To_Unbounded_String (Prefix & "Call(" & To_String (Node.Call_Name) & "[");
            begin
               for I in 1 .. Natural (Node.Call_Args.Length) loop
                  if I > 1 then
                     Append (Result, ", ");
                  end if;
                  Append (Result, AST_To_String (Node.Call_Args.Element (I), 0));
               end loop;
               Append (Result, "])");
               return To_String (Result);
            end;

         when AST_Match =>
            return Prefix & "Match(" & AST_To_String (Node.Match_Pattern, 0) &
                   " = " & AST_To_String (Node.Match_Expr, 0) & ")";

         when others =>
            return Prefix & "<" & AST_Node_Type'Image (Node.Node_Type) & ">";
      end case;
   end AST_To_String;

end Erlang_AST;
