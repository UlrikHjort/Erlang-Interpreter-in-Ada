-- ***************************************************************************
--                Erlang Pattern Matcher
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

package body Erlang_Pattern is

   --
   -- Match a value against a pattern, binding variables in the environment
   -- Returns True if match succeeds, False otherwise
   --
   function Match (Pattern : AST_Ptr; Value : Value_Ptr; Env : Environment_Ptr) return Boolean is
   begin
      if Pattern = null or Value = null then
         return False;
      end if;

      case Pattern.Node_Type is
         when AST_Variable =>
            -- Variable always matches and binds
            declare
               Var_Name : constant String := To_String (Pattern.Var_Name);
            begin
               -- Underscore is wildcard - matches anything without binding
               if Var_Name = "_" then
                  return True;
               end if;

               -- Check if variable is already bound
               if Is_Bound (Env, Var_Name) then
                  -- Must match existing binding
                  declare
                     Existing : constant Value_Ptr := Lookup (Env, Var_Name);
                  begin
                     -- Simple equality check
                     if Existing.VType /= Value.VType then
                        return False;
                     end if;

                     case Existing.VType is
                        when VT_Integer =>
                           return Existing.Int_Val = Value.Int_Val;
                        when VT_Float =>
                           return Existing.Float_Val = Value.Float_Val;
                        when VT_Atom =>
                           return Existing.Atom_Val = Value.Atom_Val;
                        when VT_String =>
                           return Existing.Str_Val = Value.Str_Val;
                        when others =>
                           return False;
                     end case;
                  end;
               else
                  -- Bind the variable
                  Bind (Env, Var_Name, Value);
                  return True;
               end if;
            end;

         when AST_Integer =>
            return Value.VType = VT_Integer and then Value.Int_Val = Pattern.Int_Value;

         when AST_Float =>
            return Value.VType = VT_Float and then Value.Float_Val = Pattern.Float_Value;

         when AST_Atom =>
            return Value.VType = VT_Atom and then Value.Atom_Val = Pattern.Atom_Name;

         when AST_String =>
            return Value.VType = VT_String and then Value.Str_Val = Pattern.Str_Value;

         when AST_Tuple =>
            if Value.VType /= VT_Tuple then
               return False;
            end if;

            if Natural (Pattern.Tuple_Elements.Length) /= Natural (Value.Tuple_Elements.Length) then
               return False;
            end if;

            -- Match each element
            for I in 1 .. Natural (Pattern.Tuple_Elements.Length) loop
               if not Match (Pattern.Tuple_Elements.Element (I), Value.Tuple_Elements.Element (I), Env) then
                  return False;
               end if;
            end loop;
            return True;

         when AST_List =>
            if Value.VType /= VT_List then
               return False;
            end if;

            if Natural (Pattern.List_Elements.Length) /= Natural (Value.List_Elements.Length) then
               return False;
            end if;

            -- Match each element
            for I in 1 .. Natural (Pattern.List_Elements.Length) loop
               if not Match (Pattern.List_Elements.Element (I), Value.List_Elements.Element (I), Env) then
                  return False;
               end if;
            end loop;
            return True;

         when AST_Cons =>
            if Value.VType /= VT_List then
               return False;
            end if;

            if Natural (Value.List_Elements.Length) = 0 then
               return False;
            end if;

            -- Match head
            if not Match (Pattern.Cons_Head, Value.List_Elements.Element (1), Env) then
               return False;
            end if;

            -- Match tail (rest of list)
            declare
               Tail_Elements : Value_Vectors.Vector;
            begin
               for I in 2 .. Natural (Value.List_Elements.Length) loop
                  Tail_Elements.Append (Value.List_Elements.Element (I));
               end loop;

               declare
                  Tail_Value : constant Value_Ptr := new Erlang_Value'(VType => VT_List, List_Elements => Tail_Elements);
               begin
                  return Match (Pattern.Cons_Tail, Tail_Value, Env);
               end;
            end;

         when others =>
            return False;
      end case;
   end Match;

end Erlang_Pattern;
