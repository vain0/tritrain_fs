namespace TriTrain.Cli

open TriTrain.Core
open System
open System.IO
open System.Xml
open System.Xml.Linq
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

  let localOut (newWriter: TextWriter) (f: unit -> 't): 't =
    let curWriter = Console.Out
    try
      Console.SetOut(newWriter)
      f ()
    finally
      Console.SetOut(curWriter)

  let parseCommandLine =
    function
    | null -> fail "No command."
    | line ->
        (line: string).Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> pass

module Xml =
  let xname (xs: string): XName =
    XName.Get(xs)

  let value (x: XElement) =
    x.Value

  let element xs (x: XElement) =
    x.Element(xname xs)

  let elements xs (x: XElement): seq<XElement> =
    x.Elements(xname xs)

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
