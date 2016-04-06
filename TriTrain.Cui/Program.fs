namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Cui.TestBattle
open TriTrain.Cui.PrintPreset
open System
open Chessie.ErrorHandling

module Program =
  let usage () =
    """
help                    Print this
show deck1 deck2        Show a battle deck1 vs deck2
test deck1 deck2        Simutate battles between deck1 and deck2
rr   decks...           Simulate round-robin tournament with decks
effs                    Show preset effects
"""

  let (|RoundRobin|_|) =
    function
    | "rr" | "round-robin" -> Some ()
    | _ -> None

  let rec procCommandArgs =
    function
    | [] ->
        trial {
          let! args = Console.ReadLine() |> Console.parseCommandLine
          return! args |> procCommandArgs
        }

    | ["show"] ->
        procCommandArgs ("show" :: defaultDeckPaths)
    | "show" :: deckPath1 :: deckPath2 :: _ ->
        showGame (deckPath1, deckPath2)

    | ["test"] ->
        procCommandArgs ("test" :: defaultDeckPaths)
    | "test" :: deckPath1 :: deckPath2 :: _ ->
        testBattleCommand (deckPath1, deckPath2)

    | [RoundRobin] ->
        procCommandArgs ("round-robin" :: defaultDeckPaths)
     | RoundRobin :: deckPaths ->
        roundRobinCommand deckPaths

    | ["effs"] ->
        showEffectsCommand ()

    | _ ->
        printfn "%s" (usage ()) |> pass

  [<EntryPoint>]
  let main argv =
    procCommandArgs (argv |> Array.toList)
    |> Trial.runConsoleApp
