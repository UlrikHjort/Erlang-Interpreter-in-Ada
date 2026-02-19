-- ***************************************************************************
--              Erlang Types and Runtime Values
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

package body Erlang_Types is

   --
   -- Create a new integer value
   --
   function Make_Integer (Val : Integer) return Value_Ptr is
   begin
      return new Erlang_Value'(VType => VT_Integer, Int_Val => Val);
   end Make_Integer;

   --
   -- Create a new float value
   --
   function Make_Float (Val : Float) return Value_Ptr is
   begin
      return new Erlang_Value'(VType => VT_Float, Float_Val => Val);
   end Make_Float;

   --
   -- Create a new atom value
   --
   function Make_Atom (Val : String) return Value_Ptr is
   begin
      return new Erlang_Value'(VType => VT_Atom, Atom_Val => To_Unbounded_String (Val));
   end Make_Atom;

   --
   -- Create a new string value
   --
   function Make_String (Val : String) return Value_Ptr is
   begin
      return new Erlang_Value'(VType => VT_String, Str_Val => To_Unbounded_String (Val));
   end Make_String;

   --
   -- Create a nil value (empty list)
   --
   function Make_Nil return Value_Ptr is
   begin
      return new Erlang_Value'(VType => VT_Nil);
   end Make_Nil;

   --
   -- Convert value to string representation
   --
   function To_String (Val : Value_Ptr) return String is
   begin
      if Val = null then
         return "null";
      end if;

      case Val.VType is
         when VT_Integer =>
            return Integer'Image (Val.Int_Val);
         when VT_Float =>
            return Float'Image (Val.Float_Val);
         when VT_Atom =>
            return To_String (Val.Atom_Val);
         when VT_String =>
            return """" & To_String (Val.Str_Val) & """";
         when VT_Tuple =>
            declare
               Result : Unbounded_String := To_Unbounded_String ("{");
            begin
               for I in 1 .. Natural (Val.Tuple_Elements.Length) loop
                  if I > 1 then
                     Append (Result, ", ");
                  end if;
                  Append (Result, To_String (Val.Tuple_Elements.Element (I)));
               end loop;
               Append (Result, "}");
               return To_String (Result);
            end;

         when VT_List =>
            declare
               Result : Unbounded_String := To_Unbounded_String ("[");
            begin
               for I in 1 .. Natural (Val.List_Elements.Length) loop
                  if I > 1 then
                     Append (Result, ", ");
                  end if;
                  Append (Result, To_String (Val.List_Elements.Element (I)));
               end loop;
               Append (Result, "]");
               return To_String (Result);
            end;
         when VT_Function =>
            return To_String (Val.Func_Name) & "/" & Natural'Image (Val.Arity);
         when VT_Nil =>
            return "[]";
      end case;
   end To_String;

end Erlang_Types;
