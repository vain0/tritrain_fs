namespace TriTrain.Cui

open TriTrain.Core
open System
open Chessie.ErrorHandling

module Console =
  open Printf

  let mcprintf (f: string -> string) (color': ConsoleColor) =
    kprintf (fun text ->
      let color = Console.ForegroundColor
      let () = Console.ForegroundColor <- color'
      let () = Console.Write(f text)
      let () = Console.ForegroundColor <- color
      in ())

  let mcprintfn f color' =
    mcprintf (f >> flip (+) Environment.NewLine) color'

  let parseCommandLine =
    function
    | null -> fail "No command."
    | line ->
        (line: string).Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> pass

module Trial =
  /// Shows warning/error messages.
  /// Returns exit code.
  let runConsoleApp r: int =
    match r with
    | Pass () ->
        0
    | Warn ((), msgs) ->
        eprintfn "Warning:"
        msgs |> List.iter (eprintfn "%s")
        0
    | Fail msgs ->
        eprintfn "Error:"
        msgs |> List.iter (eprintfn "%s")
        1
