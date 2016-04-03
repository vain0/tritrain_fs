namespace TriTrain.Cui

open TriTrain.Core
open System

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
