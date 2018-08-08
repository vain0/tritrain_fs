namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Cui.Random
open TriTrain.Cui.TestBattle
open System
open Chessie.ErrorHandling

module Program =
  let usage () =
    """
help                    Print this
show deck1 deck2        Show a battle deck1 vs deck2
test deck1 deck2        Simulate battles between deck1 and deck2
rr   decks...           Simulate round-robin tournament with decks
rand                    Generate random deck
effs                    Show preset effects
cake username password  Run interactive tritrain_cake client
"""

  let rec procCommandArgs =
    function
    | [] ->
        trial {
          let! args = Console.ReadLine() |> Console.parseCommandLine
          return! args |> procCommandArgs
        }

    | ShowGame r -> r
    | TestBattle r -> r
    | RoundRobin r -> r
    | Random r -> r
    | Cake.Command r -> r

    | ["effs"] ->
        trial {
          do printfn "%s" (Dump.dumpPresetSkillsToMarkdown ())
          do printfn "%s" (Dump.dumpPresetAbilsToMarkdown ())
        }

    | _ ->
        printfn "%s" (usage ()) |> pass

  [<EntryPoint>]
  let main argv =
    procCommandArgs (argv |> Array.toList)
    |> Trial.runConsoleApp
