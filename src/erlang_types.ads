-- ***************************************************************************
--               Erlang Types and Runtime Values
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

package Erlang_Types is

   -- Runtime value types for dynamic typing
   type Value_Type is (
      VT_Integer,
      VT_Float,
      VT_Atom,
      VT_String,
      VT_Tuple,
      VT_List,
      VT_Function,
      VT_Nil
   );

   type Erlang_Value;
   type Value_Ptr is access Erlang_Value;

   package Value_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Value_Ptr);

   -- Main runtime value discriminated record
   type Erlang_Value (VType : Value_Type := VT_Nil) is record
      case VType is
         when VT_Integer =>
            Int_Val : Integer;
         when VT_Float =>
            Float_Val : Float;
         when VT_Atom =>
            Atom_Val : Unbounded_String;
         when VT_String =>
            Str_Val : Unbounded_String;
         when VT_Tuple =>
            Tuple_Elements : Value_Vectors.Vector;
         when VT_List =>
            List_Elements : Value_Vectors.Vector;
         when VT_Function =>
            Func_Name : Unbounded_String;
            Arity : Natural;
         when VT_Nil =>
            null;
      end case;
   end record;

   --
   -- Create a new integer value
   --
   function Make_Integer (Val : Integer) return Value_Ptr;

   --
   -- Create a new float value
   --
   function Make_Float (Val : Float) return Value_Ptr;

   --
   -- Create a new atom value
   --
   function Make_Atom (Val : String) return Value_Ptr;

   --
   -- Create a new string value
   --
   function Make_String (Val : String) return Value_Ptr;

   --
   -- Create a nil value (empty list)
   --
   function Make_Nil return Value_Ptr;

   --
   -- Convert value to string representation
   --
   function To_String (Val : Value_Ptr) return String;

end Erlang_Types;
