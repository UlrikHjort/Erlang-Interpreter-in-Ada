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

package body Erlang_Environment is

   --
   -- Create a new empty environment
   --
   function New_Environment return Environment_Ptr is
   begin
      return new Environment;
   end New_Environment;

   --
   -- Bind a variable to a value
   --
   procedure Bind (Env : Environment_Ptr; Name : String; Value : Value_Ptr) is
      Key : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      Env.Bindings.Include (Key, Value);
   end Bind;

   --
   -- Lookup a variable value
   --
   function Lookup (Env : Environment_Ptr; Name : String) return Value_Ptr is
      Key : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if Env.Bindings.Contains (Key) then
         return Env.Bindings.Element (Key);
      else
         raise Unbound_Variable with "Variable '" & Name & "' is not bound";
      end if;
   end Lookup;

   --
   -- Check if variable is bound
   --
   function Is_Bound (Env : Environment_Ptr; Name : String) return Boolean is
      Key : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      return Env.Bindings.Contains (Key);
   end Is_Bound;

   --
   -- Define a function in the environment
   --
   procedure Define_Function (Env : Environment_Ptr; Name : String; Func : AST_Ptr) is
      Key : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      Env.Functions.Include (Key, Func);
   end Define_Function;

   --
   -- Lookup a function definition
   --
   function Lookup_Function (Env : Environment_Ptr; Name : String) return AST_Ptr is
      Key : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if Env.Functions.Contains (Key) then
         return Env.Functions.Element (Key);
      else
         raise Undefined_Function with "Function '" & Name & "' is not defined";
      end if;
   end Lookup_Function;

   --
   -- Check if function is defined
   --
   function Is_Function_Defined (Env : Environment_Ptr; Name : String) return Boolean is
      Key : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      return Env.Functions.Contains (Key);
   end Is_Function_Defined;

end Erlang_Environment;
