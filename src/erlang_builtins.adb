-- ***************************************************************************
--         Erlang Built-in Functions (BIFs) Implementation
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

with Ada.Text_IO;

package body Erlang_Builtins is

   -- Check if a function name is a built-in
   function Is_Builtin (Name : String) return Boolean is
   begin
      return Name = "length" or else
             Name = "hd" or else
             Name = "tl" or else
             Name = "is_integer" or else
             Name = "is_float" or else
             Name = "is_atom" or else
             Name = "is_list" or else
             Name = "is_tuple" or else
             Name = "print" or else
             Name = "println";
   end Is_Builtin;

   -- Built-in function: length/1 - Get length of list
   function BIF_Length (Args : Value_Vectors.Vector) return Value_Ptr is
   begin
      if Natural (Args.Length) /= 1 then
         raise Builtin_Error with "length/1 expects 1 argument";
      end if;

      declare
         Val : constant Value_Ptr := Args.Element (1);
      begin
         if Val.VType /= VT_List then
            raise Builtin_Error with "length/1 expects a list";
         end if;
         return Make_Integer (Integer (Val.List_Elements.Length));
      end;
   end BIF_Length;

   -- Built-in function: hd/1 - Get head of list
   function BIF_Hd (Args : Value_Vectors.Vector) return Value_Ptr is
   begin
      if Natural (Args.Length) /= 1 then
         raise Builtin_Error with "hd/1 expects 1 argument";
      end if;

      declare
         Val : constant Value_Ptr := Args.Element (1);
      begin
         if Val.VType /= VT_List then
            raise Builtin_Error with "hd/1 expects a list";
         end if;
         if Natural (Val.List_Elements.Length) = 0 then
            raise Builtin_Error with "hd/1 called on empty list";
         end if;
         return Val.List_Elements.Element (1);
      end;
   end BIF_Hd;

   -- Built-in function: tl/1 - Get tail of list
   function BIF_Tl (Args : Value_Vectors.Vector) return Value_Ptr is
   begin
      if Natural (Args.Length) /= 1 then
         raise Builtin_Error with "tl/1 expects 1 argument";
      end if;

      declare
         Val : constant Value_Ptr := Args.Element (1);
      begin
         if Val.VType /= VT_List then
            raise Builtin_Error with "tl/1 expects a list";
         end if;
         if Natural (Val.List_Elements.Length) = 0 then
            raise Builtin_Error with "tl/1 called on empty list";
         end if;

         declare
            Result : Value_Vectors.Vector;
         begin
            for I in 2 .. Natural (Val.List_Elements.Length) loop
               Result.Append (Val.List_Elements.Element (I));
            end loop;
            return new Erlang_Value'(VType => VT_List, List_Elements => Result);
         end;
      end;
   end BIF_Tl;

   -- Built-in function: is_integer/1 - Check if value is integer
   function BIF_Is_Integer (Args : Value_Vectors.Vector) return Value_Ptr is
   begin
      if Natural (Args.Length) /= 1 then
         raise Builtin_Error with "is_integer/1 expects 1 argument";
      end if;

      if Args.Element (1).VType = VT_Integer then
         return Make_Atom ("true");
      else
         return Make_Atom ("false");
      end if;
   end BIF_Is_Integer;

   -- Built-in function: is_float/1 - Check if value is float
   function BIF_Is_Float (Args : Value_Vectors.Vector) return Value_Ptr is
   begin
      if Natural (Args.Length) /= 1 then
         raise Builtin_Error with "is_float/1 expects 1 argument";
      end if;

      if Args.Element (1).VType = VT_Float then
         return Make_Atom ("true");
      else
         return Make_Atom ("false");
      end if;
   end BIF_Is_Float;

   -- Built-in function: is_atom/1 - Check if value is atom
   function BIF_Is_Atom (Args : Value_Vectors.Vector) return Value_Ptr is
   begin
      if Natural (Args.Length) /= 1 then
         raise Builtin_Error with "is_atom/1 expects 1 argument";
      end if;

      if Args.Element (1).VType = VT_Atom then
         return Make_Atom ("true");
      else
         return Make_Atom ("false");
      end if;
   end BIF_Is_Atom;

   -- Built-in function: is_list/1 - Check if value is list
   function BIF_Is_List (Args : Value_Vectors.Vector) return Value_Ptr is
   begin
      if Natural (Args.Length) /= 1 then
         raise Builtin_Error with "is_list/1 expects 1 argument";
      end if;

      if Args.Element (1).VType = VT_List then
         return Make_Atom ("true");
      else
         return Make_Atom ("false");
      end if;
   end BIF_Is_List;

   -- Built-in function: is_tuple/1 - Check if value is tuple
   function BIF_Is_Tuple (Args : Value_Vectors.Vector) return Value_Ptr is
   begin
      if Natural (Args.Length) /= 1 then
         raise Builtin_Error with "is_tuple/1 expects 1 argument";
      end if;

      if Args.Element (1).VType = VT_Tuple then
         return Make_Atom ("true");
      else
         return Make_Atom ("false");
      end if;
   end BIF_Is_Tuple;

   -- Built-in function: print/1 - Print value without newline
   function BIF_Print (Args : Value_Vectors.Vector) return Value_Ptr is
   begin
      if Natural (Args.Length) /= 1 then
         raise Builtin_Error with "print/1 expects 1 argument";
      end if;

      Ada.Text_IO.Put (Erlang_Types.To_String (Args.Element (1)));
      return Make_Atom ("ok");
   end BIF_Print;

   -- Built-in function: println/1 - Print value with newline
   function BIF_Println (Args : Value_Vectors.Vector) return Value_Ptr is
   begin
      if Natural (Args.Length) /= 1 then
         raise Builtin_Error with "println/1 expects 1 argument";
      end if;

      Ada.Text_IO.Put_Line (Erlang_Types.To_String (Args.Element (1)));
      return Make_Atom ("ok");
   end BIF_Println;

   -- Call a built-in function
   function Call_Builtin (Name : String; Args : Value_Vectors.Vector) return Value_Ptr is
   begin
      if Name = "length" then
         return BIF_Length (Args);
      elsif Name = "hd" then
         return BIF_Hd (Args);
      elsif Name = "tl" then
         return BIF_Tl (Args);
      elsif Name = "is_integer" then
         return BIF_Is_Integer (Args);
      elsif Name = "is_float" then
         return BIF_Is_Float (Args);
      elsif Name = "is_atom" then
         return BIF_Is_Atom (Args);
      elsif Name = "is_list" then
         return BIF_Is_List (Args);
      elsif Name = "is_tuple" then
         return BIF_Is_Tuple (Args);
      elsif Name = "print" then
         return BIF_Print (Args);
      elsif Name = "println" then
         return BIF_Println (Args);
      else
         raise Builtin_Error with "Unknown built-in function: " & Name;
      end if;
   end Call_Builtin;

end Erlang_Builtins;
