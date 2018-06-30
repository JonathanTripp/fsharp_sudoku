module Sudoku.Repl.console.Console_win

open Sudoku.Lib.core.Sudoku

open System
open System.Diagnostics
open System.Runtime.InteropServices

[<DllImport("user32.dll")>]
extern bool ShowWindow(System.IntPtr hWnd, int cmdShow)

let cs (c : char) : Format.consoleString = [Format.CChar c]

(* Some predefined characters - for smaller grid *)
let defaultGridChars : Format.gridChars = 
    { h = cs '─';
      v = 
          { l = cs '│';
            m = cs '│';
            r = cs '│' };
      t = 
          { l = cs '┌';
            m = cs '┬';
            r = cs '┐' };
      m = 
          { l = cs '├';
            m = cs '┼';
            r = cs '┤' };
      b = 
          { l = cs '└';
            m = cs '┴';
            r = cs '┘' };
      n = [Format.NL] }

(* Some predefined characters - for smaller grid *)
let defaultCandidateGridChars : Format.candidateGridChars = 
    { h = cs '═';
      hi = cs '─';
      v = 
          { l = cs '║';
            m = cs '║';
            r = cs '║' };
      vi = cs '│';
      t = 
          { mi = cs '╦';
            x = 
                { l = cs '╔';
                  m = cs '╦';
                  r = cs '╗' } };
      m = 
          { mi = cs '╬';
            x = 
                { l = cs '╠';
                  m = cs '╬';
                  r = cs '╣' } };
      mi = 
          { mi = cs '┼';
            x = 
                { l = cs '╠';
                  m = cs '╬';
                  r = cs '╣' } };
      b = 
          { mi = cs '╧';
            x = 
                { l = cs '╚';
                  m = cs '╩';
                  r = cs '╝' } };
      n = [Format.NL] }

let basic_to_system (color : Format.basic_color) : ConsoleColor =
    match color with
    | Format.DefaultColour -> ConsoleColor.White
    | Format.Black -> ConsoleColor.Black
    | Format.Red -> ConsoleColor.Red
    | Format.Green -> ConsoleColor.Green
    | Format.Yellow -> ConsoleColor.Yellow
    | Format.Blue -> ConsoleColor.Blue
    | Format.Magenta -> ConsoleColor.Magenta
    | Format.Cyan -> ConsoleColor.Cyan
    | Format.White -> ConsoleColor.White

(* Change the console colour to write a string *)
let consoleWriteColor (foreground_colour : Format.basic_color) (background_colour : Format.basic_color) (value : 'a) = 
    match foreground_colour, background_colour with
    | Format.DefaultColour, Format.DefaultColour ->
        System.Console.Write value
    | _, Format.DefaultColour ->
        let foregroundColour = System.Console.ForegroundColor in
        System.Console.ForegroundColor <- basic_to_system foreground_colour;
        System.Console.Write value;
        System.Console.ForegroundColor <- foregroundColour
    | Format.DefaultColour, _ ->
        let backgroundColour = System.Console.BackgroundColor in
        System.Console.BackgroundColor <- basic_to_system background_colour;
        System.Console.Write value;
        System.Console.BackgroundColor <- backgroundColour
    | _ ->
        let foregroundColour = System.Console.ForegroundColor in
        let backgroundColour = System.Console.BackgroundColor in
        System.Console.ForegroundColor <- basic_to_system foreground_colour;
        System.Console.BackgroundColor <- basic_to_system background_colour;
        System.Console.Write value;
        System.Console.ForegroundColor <- foregroundColour;
        System.Console.BackgroundColor <- backgroundColour

let drawConsoleChar (consoleChar : Format.consoleChar) = 
    match consoleChar with
    | Format.CNil -> ()
    | Format.CChar c -> Console.Write c
    | Format.CStr c -> Console.Write c
    | Format.CDigit (Digit d) -> Console.Write d
    | Format.ColouredString(c, foreground_colour, background_colour) -> consoleWriteColor foreground_colour background_colour c
    | Format.ColouredDigit ((Digit d), foreground_colour, background_colour) -> consoleWriteColor foreground_colour background_colour d
    | Format.NL -> Console.WriteLine ""

let drawConsoleString (consoleString : Format.consoleString) : unit =
    List.iter drawConsoleChar consoleString

let maximise_console() : unit =
    let p = Process.GetCurrentProcess() in
    ShowWindow(p.MainWindowHandle, 3) (* SW_MAXIMIZE = 3 *)
    |> ignore
