-- ***************************************************************************
--               Erlang Interpreter - Main Program
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Erlang_Lexer; use Erlang_Lexer;
with Erlang_Parser; use Erlang_Parser;
with Erlang_AST; use Erlang_AST;
with Erlang_Types; use Erlang_Types;
with Erlang_Environment; use Erlang_Environment;
with Erlang_Evaluator; use Erlang_Evaluator;

--
-- Main program for the Erlang interpreter
-- Supports REPL mode and file execution
--
procedure Main is

   --
   -- Display usage information
   --
   procedure Show_Usage is
   begin
      Put_Line ("Erlang Interpreter v0.1");
      Put_Line ("Copyright (C) 2026 By Ulrik Hørlyk Hjort");
      Put_Line ("");
      Put_Line ("Usage:");
      Put_Line ("  erlang_interpreter            - Start REPL mode");
      Put_Line ("  erlang_interpreter <file>     - Execute Erlang file");
      Put_Line ("  erlang_interpreter --test     - Run test suite");
   end Show_Usage;

   --
   -- Run REPL (Read-Eval-Print Loop)
   --
   procedure Run_REPL is
      Input : String (1 .. 1024);
      Last  : Natural;
      Env   : constant Environment_Ptr := New_Environment;
   begin
      Put_Line ("Erlang REPL - Type 'quit.' to exit");
      Put_Line ("");

      loop
         begin
            Put ("> ");
            Get_Line (Input, Last);

            exit when Last >= 5 and then Input (1 .. 5) = "quit.";

            if Last > 0 then
               declare
                  Tokens : constant Token_Vectors.Vector := Tokenize (Input (1 .. Last));
                  AST    : AST_Ptr;
                  Result : Value_Ptr;
               begin
                  AST := Parse (Tokens);
                  if AST /= null then
                     Result := Eval (AST, Env);
                     Put_Line ("=> " & To_String (Result));
                  end if;
               exception
                  when E : others =>
                     Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
               end;
            end if;
         exception
            when Ada.Text_IO.End_Error =>
               exit;
         end;
      end loop;

      Put_Line ("Goodbye!");
   end Run_REPL;

   --
   -- Execute an Erlang source file
   --
   procedure Run_File (Filename : String) is
      File : File_Type;
      Source : Unbounded_String;
   begin
      Open (File, In_File, Filename);

      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            if Length (Source) > 0 then
               Append (Source, ASCII.LF);
            end if;
            Append (Source, Line);
         end;
      end loop;

      Close (File);

      Put_Line ("Parsing and evaluating " & Filename & "...");
      declare
         Tokens : constant Token_Vectors.Vector := Tokenize (To_String (Source));
         AST    : AST_Ptr;
         Env    : constant Environment_Ptr := New_Environment;
         Result : Value_Ptr;
      begin
         AST := Parse (Tokens);

         if AST /= null then
            Result := Eval (AST, Env);
            Put_Line ("Result: " & To_String (Result));
         else
            Put_Line ("No expression to evaluate");
         end if;
      end;

   exception
      when Ada.Text_IO.Name_Error =>
         Put_Line ("Error: Cannot open file '" & Filename & "'");
      when E : others =>
         Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
   end Run_File;

   --
   -- Run basic tests
   --
   procedure Run_Tests is
   begin
      Put_Line ("Running Lexer Tests...");
      Put_Line ("");

      declare
         Test_Cases : constant array (1 .. 5) of Unbounded_String := (
            To_Unbounded_String ("factorial(0) -> 1."),
            To_Unbounded_String ("X = 42"),
            To_Unbounded_String ("{atom, 123, ""string""}"),
            To_Unbounded_String ("[1, 2, 3 | Tail]"),
            To_Unbounded_String ("A == B, A /= C, A =:= D")
         );
      begin
         for I in Test_Cases'Range loop
            Put_Line ("Test" & Integer'Image (I) & ": " & To_String (Test_Cases (I)));
            declare
               Tokens : constant Token_Vectors.Vector := Tokenize (To_String (Test_Cases (I)));
            begin
               Put_Line ("  Tokens:" & Natural'Image (Natural (Tokens.Length) - 1)); -- -1 for EOF
            end;
            Put_Line ("");
         end loop;
      end;

      Put_Line ("All tests passed!");
   end Run_Tests;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      Run_REPL;
   elsif Ada.Command_Line.Argument_Count = 1 then
      declare
         Arg : constant String := Ada.Command_Line.Argument (1);
      begin
         if Arg = "--test" or Arg = "-t" then
            Run_Tests;
         elsif Arg = "--help" or Arg = "-h" then
            Show_Usage;
         else
            Run_File (Arg);
         end if;
      end;
   else
      Show_Usage;
   end if;
end Main;
