-- ***************************************************************************
--            Erlang Environment - Variable bindings
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
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Erlang_Types; use Erlang_Types;
with Erlang_AST; use Erlang_AST;

package Erlang_Environment is

   Unbound_Variable : exception;
   Undefined_Function : exception;

   -- Hash map for variable bindings
   package Binding_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Value_Ptr,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   -- Hash map for function definitions (name -> AST)
   package Function_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => AST_Ptr,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   type Environment is record
      Bindings  : Binding_Maps.Map;
      Functions : Function_Maps.Map;
   end record;

   type Environment_Ptr is access Environment;

   --
   -- Create a new empty environment
   --
   function New_Environment return Environment_Ptr;

   --
   -- Bind a variable to a value
   --
   procedure Bind (Env : Environment_Ptr; Name : String; Value : Value_Ptr);

   --
   -- Lookup a variable value
   --
   function Lookup (Env : Environment_Ptr; Name : String) return Value_Ptr;

   --
   -- Check if variable is bound
   --
   function Is_Bound (Env : Environment_Ptr; Name : String) return Boolean;

   --
   -- Define a function in the environment
   --
   procedure Define_Function (Env : Environment_Ptr; Name : String; Func : AST_Ptr);

   --
   -- Lookup a function definition
   --
   function Lookup_Function (Env : Environment_Ptr; Name : String) return AST_Ptr;

   --
   -- Check if function is defined
   --
   function Is_Function_Defined (Env : Environment_Ptr; Name : String) return Boolean;

end Erlang_Environment;
