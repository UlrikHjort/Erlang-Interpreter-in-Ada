-- ***************************************************************************
--          Erlang Evaluator - Execute AST expressions
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

with Erlang_Pattern; use Erlang_Pattern;
with Erlang_Builtins;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Erlang_Evaluator is

   --
   -- Evaluate binary operation on two values
   --
   function Eval_Binary_Op (Op : Binary_Operator; Left : Value_Ptr; Right : Value_Ptr) return Value_Ptr is
   begin
      case Op is
         when OP_Add =>
            if Left.VType = VT_Integer and Right.VType = VT_Integer then
               return Make_Integer (Left.Int_Val + Right.Int_Val);
            elsif Left.VType = VT_Float and Right.VType = VT_Float then
               return Make_Float (Left.Float_Val + Right.Float_Val);
            elsif Left.VType = VT_Integer and Right.VType = VT_Float then
               return Make_Float (Float (Left.Int_Val) + Right.Float_Val);
            elsif Left.VType = VT_Float and Right.VType = VT_Integer then
               return Make_Float (Left.Float_Val + Float (Right.Int_Val));
            else
               raise Type_Error with "Invalid types for addition";
            end if;

         when OP_Sub =>
            if Left.VType = VT_Integer and Right.VType = VT_Integer then
               return Make_Integer (Left.Int_Val - Right.Int_Val);
            elsif Left.VType = VT_Float and Right.VType = VT_Float then
               return Make_Float (Left.Float_Val - Right.Float_Val);
            elsif Left.VType = VT_Integer and Right.VType = VT_Float then
               return Make_Float (Float (Left.Int_Val) - Right.Float_Val);
            elsif Left.VType = VT_Float and Right.VType = VT_Integer then
               return Make_Float (Left.Float_Val - Float (Right.Int_Val));
            else
               raise Type_Error with "Invalid types for subtraction";
            end if;

         when OP_Mul =>
            if Left.VType = VT_Integer and Right.VType = VT_Integer then
               return Make_Integer (Left.Int_Val * Right.Int_Val);
            elsif Left.VType = VT_Float and Right.VType = VT_Float then
               return Make_Float (Left.Float_Val * Right.Float_Val);
            elsif Left.VType = VT_Integer and Right.VType = VT_Float then
               return Make_Float (Float (Left.Int_Val) * Right.Float_Val);
            elsif Left.VType = VT_Float and Right.VType = VT_Integer then
               return Make_Float (Left.Float_Val * Float (Right.Int_Val));
            else
               raise Type_Error with "Invalid types for multiplication";
            end if;

         when OP_Divide =>
            if Left.VType = VT_Integer and Right.VType = VT_Integer then
               if Right.Int_Val = 0 then
                  raise Evaluation_Error with "Division by zero";
               end if;
               return Make_Float (Float (Left.Int_Val) / Float (Right.Int_Val));
            elsif Left.VType = VT_Float and Right.VType = VT_Float then
               if Right.Float_Val = 0.0 then
                  raise Evaluation_Error with "Division by zero";
               end if;
               return Make_Float (Left.Float_Val / Right.Float_Val);
            elsif Left.VType = VT_Integer and Right.VType = VT_Float then
               if Right.Float_Val = 0.0 then
                  raise Evaluation_Error with "Division by zero";
               end if;
               return Make_Float (Float (Left.Int_Val) / Right.Float_Val);
            elsif Left.VType = VT_Float and Right.VType = VT_Integer then
               if Right.Int_Val = 0 then
                  raise Evaluation_Error with "Division by zero";
               end if;
               return Make_Float (Left.Float_Val / Float (Right.Int_Val));
            else
               raise Type_Error with "Invalid types for division";
            end if;

         when OP_Div =>
            if Left.VType = VT_Integer and Right.VType = VT_Integer then
               if Right.Int_Val = 0 then
                  raise Evaluation_Error with "Division by zero";
               end if;
               return Make_Integer (Left.Int_Val / Right.Int_Val);
            else
               raise Type_Error with "div requires integer operands";
            end if;

         when OP_Mod =>
            if Left.VType = VT_Integer and Right.VType = VT_Integer then
               if Right.Int_Val = 0 then
                  raise Evaluation_Error with "Modulo by zero";
               end if;
               return Make_Integer (Left.Int_Val mod Right.Int_Val);
            else
               raise Type_Error with "mod requires integer operands";
            end if;

         when OP_Eq | OP_Exact_Eq =>
            if Left.VType /= Right.VType then
               return Make_Atom ("false");
            end if;

            case Left.VType is
               when VT_Integer =>
                  if Left.Int_Val = Right.Int_Val then
                     return Make_Atom ("true");
                  else
                     return Make_Atom ("false");
                  end if;
               when VT_Float =>
                  if Left.Float_Val = Right.Float_Val then
                     return Make_Atom ("true");
                  else
                     return Make_Atom ("false");
                  end if;
               when VT_Atom =>
                  if Left.Atom_Val = Right.Atom_Val then
                     return Make_Atom ("true");
                  else
                     return Make_Atom ("false");
                  end if;
               when VT_String =>
                  if Left.Str_Val = Right.Str_Val then
                     return Make_Atom ("true");
                  else
                     return Make_Atom ("false");
                  end if;
               when others =>
                  return Make_Atom ("false");
            end case;

         when OP_Neq | OP_Exact_Neq =>
            declare
               Eq_Result : constant Value_Ptr := Eval_Binary_Op (OP_Eq, Left, Right);
            begin
               if To_String (Eq_Result.Atom_Val) = "true" then
                  return Make_Atom ("false");
               else
                  return Make_Atom ("true");
               end if;
            end;

         when OP_Lt =>
            if Left.VType = VT_Integer and Right.VType = VT_Integer then
               if Left.Int_Val < Right.Int_Val then
                  return Make_Atom ("true");
               else
                  return Make_Atom ("false");
               end if;
            elsif Left.VType = VT_Float and Right.VType = VT_Float then
               if Left.Float_Val < Right.Float_Val then
                  return Make_Atom ("true");
               else
                  return Make_Atom ("false");
               end if;
            else
               raise Type_Error with "Invalid types for comparison";
            end if;

         when OP_Gt =>
            if Left.VType = VT_Integer and Right.VType = VT_Integer then
               if Left.Int_Val > Right.Int_Val then
                  return Make_Atom ("true");
               else
                  return Make_Atom ("false");
               end if;
            elsif Left.VType = VT_Float and Right.VType = VT_Float then
               if Left.Float_Val > Right.Float_Val then
                  return Make_Atom ("true");
               else
                  return Make_Atom ("false");
               end if;
            else
               raise Type_Error with "Invalid types for comparison";
            end if;

         when OP_Lte =>
            if Left.VType = VT_Integer and Right.VType = VT_Integer then
               if Left.Int_Val <= Right.Int_Val then
                  return Make_Atom ("true");
               else
                  return Make_Atom ("false");
               end if;
            elsif Left.VType = VT_Float and Right.VType = VT_Float then
               if Left.Float_Val <= Right.Float_Val then
                  return Make_Atom ("true");
               else
                  return Make_Atom ("false");
               end if;
            else
               raise Type_Error with "Invalid types for comparison";
            end if;

         when OP_Gte =>
            if Left.VType = VT_Integer and Right.VType = VT_Integer then
               if Left.Int_Val >= Right.Int_Val then
                  return Make_Atom ("true");
               else
                  return Make_Atom ("false");
               end if;
            elsif Left.VType = VT_Float and Right.VType = VT_Float then
               if Left.Float_Val >= Right.Float_Val then
                  return Make_Atom ("true");
               else
                  return Make_Atom ("false");
               end if;
            else
               raise Type_Error with "Invalid types for comparison";
            end if;
      end case;
   end Eval_Binary_Op;

   --
   -- Evaluate an AST expression in the given environment
   --
   function Eval (Node : AST_Ptr; Env : Environment_Ptr) return Value_Ptr is
   begin
      if Node = null then
         return Make_Nil;
      end if;

      case Node.Node_Type is
         when AST_Integer =>
            return Make_Integer (Node.Int_Value);

         when AST_Float =>
            return Make_Float (Node.Float_Value);

         when AST_Atom =>
            return Make_Atom (To_String (Node.Atom_Name));

         when AST_String =>
            return Make_String (To_String (Node.Str_Value));

         when AST_Variable =>
            return Lookup (Env, To_String (Node.Var_Name));

         when AST_Tuple =>
            declare
               Elements : Value_Vectors.Vector;
            begin
               for AST_Elem of Node.Tuple_Elements loop
                  Elements.Append (Eval (AST_Elem, Env));
               end loop;
               return new Erlang_Value'(VType => VT_Tuple, Tuple_Elements => Elements);
            end;

         when AST_List =>
            declare
               Elements : Value_Vectors.Vector;
            begin
               for AST_Elem of Node.List_Elements loop
                  Elements.Append (Eval (AST_Elem, Env));
               end loop;
               return new Erlang_Value'(VType => VT_List, List_Elements => Elements);
            end;

         when AST_Cons =>
            declare
               Head : constant Value_Ptr := Eval (Node.Cons_Head, Env);
               Tail : constant Value_Ptr := Eval (Node.Cons_Tail, Env);
               Elements : Value_Vectors.Vector;
            begin
               -- Build a list with head prepended to tail
               Elements.Append (Head);
               if Tail.VType = VT_List then
                  for Elem of Tail.List_Elements loop
                     Elements.Append (Elem);
                  end loop;
               end if;
               return new Erlang_Value'(VType => VT_List, List_Elements => Elements);
            end;

         when AST_Binary_Op =>
            declare
               Left  : constant Value_Ptr := Eval (Node.Bin_Left, Env);
               Right : constant Value_Ptr := Eval (Node.Bin_Right, Env);
            begin
               return Eval_Binary_Op (Node.Bin_Op, Left, Right);
            end;

         when AST_Match =>
            declare
               Value : constant Value_Ptr := Eval (Node.Match_Expr, Env);
            begin
               -- Use full pattern matching engine
               if Match (Node.Match_Pattern, Value, Env) then
                  return Value;
               else
                  raise Evaluation_Error with "Pattern match failed";
               end if;
            end;

         when AST_Function_Call =>
            declare
               Func_Name : constant String := To_String (Node.Call_Name);
            begin
               -- Evaluate arguments
               declare
                  Args : Value_Vectors.Vector;
               begin
                  for Arg_AST of Node.Call_Args loop
                     Args.Append (Eval (Arg_AST, Env));
                  end loop;

                  -- Check if it's a built-in function first
                  if Erlang_Builtins.Is_Builtin (Func_Name) then
                     return Erlang_Builtins.Call_Builtin (Func_Name, Args);
                  end if;

                  -- Look up user-defined function
                  if not Is_Function_Defined (Env, Func_Name) then
                     raise Evaluation_Error with "Undefined function: " & Func_Name;
                  end if;

                  declare
                     Func_Def : constant AST_Ptr := Lookup_Function (Env, Func_Name);
                  begin
                     -- Try each clause
                     for Clause of Func_Def.Func_Clauses loop
                        -- Check arity matches
                        if Natural (Clause.Patterns.Length) /= Natural (Args.Length) then
                           goto Next_Clause;
                        end if;

                        -- Create new environment for this clause
                        declare
                           Clause_Env : constant Environment_Ptr := New_Environment;
                           Match_Success : Boolean := True;
                        begin
                           -- Copy parent functions to clause environment
                           Clause_Env.Functions := Env.Functions;

                           -- Try to match all patterns
                           for I in 1 .. Natural (Args.Length) loop
                              if not Match (Clause.Patterns.Element (I), Args.Element (I), Clause_Env) then
                                 Match_Success := False;
                                 exit;
                              end if;
                           end loop;

                           if not Match_Success then
                              goto Next_Clause;
                           end if;

                           -- Check guard if present
                           if Clause.Guard /= null then
                              declare
                                 Guard_Result : constant Value_Ptr := Eval (Clause.Guard, Clause_Env);
                              begin
                                 if Guard_Result.VType /= VT_Atom or else To_String (Guard_Result.Atom_Val) /= "true" then
                                    goto Next_Clause;
                                 end if;
                              end;
                           end if;

                           -- Execute clause body
                           return Eval (Clause.Clause_Body, Clause_Env);
                        end;

                        <<Next_Clause>>
                     end loop;

                     raise Evaluation_Error with "No matching clause for " & Func_Name;
                  end;
               end;
            end;

         when AST_Function_Def =>
            -- Store function definition
            Define_Function (Env, To_String (Node.Func_Name), Node);
            return Make_Atom (To_String (Node.Func_Name));

         when AST_Case =>
            -- Evaluate case expression
            declare
               Case_Value : constant Value_Ptr := Eval (Node.Case_Expr, Env);
            begin
               -- Try each clause
               for Clause of Node.Case_Clauses loop
                  -- Case has single pattern per clause
                  declare
                     Clause_Env : constant Environment_Ptr := New_Environment;
                  begin
                     -- Copy parent functions and variables to clause environment
                     Clause_Env.Functions := Env.Functions;
                     Clause_Env.Bindings := Env.Bindings;

                     -- Try to match pattern
                     if Match (Clause.Patterns.Element (1), Case_Value, Clause_Env) then
                        -- Check guard if present
                        if Clause.Guard /= null then
                           declare
                              Guard_Result : constant Value_Ptr := Eval (Clause.Guard, Clause_Env);
                           begin
                              if Guard_Result.VType /= VT_Atom or else To_String (Guard_Result.Atom_Val) /= "true" then
                                 goto Next_Case_Clause;
                              end if;
                           end;
                        end if;

                        -- Execute clause body
                        return Eval (Clause.Clause_Body, Clause_Env);
                     end if;

                     <<Next_Case_Clause>>
                  end;
               end loop;

               raise Evaluation_Error with "No matching case clause";
            end;

         when AST_If =>
            -- Evaluate if expression
            for Clause of Node.If_Clauses loop
               declare
                  Guard_Result : constant Value_Ptr := Eval (Clause.Guard, Env);
               begin
                  if Guard_Result.VType = VT_Atom and then To_String (Guard_Result.Atom_Val) = "true" then
                     return Eval (Clause.Clause_Body, Env);
                  end if;
               end;
            end loop;

            raise Evaluation_Error with "No true condition in if expression";

         when others =>
            raise Evaluation_Error with "Evaluation not implemented for " &
                  AST_Node_Type'Image (Node.Node_Type);
      end case;
   end Eval;

end Erlang_Evaluator;
