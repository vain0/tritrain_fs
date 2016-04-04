namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Core.Serialize
open System
open Chessie.ErrorHandling

module Program =

  let loadDecks (deckPath1, deckPath2) =
    trial {
      let! deck1 = DeckSpecSrc.load deckPath1
      let! deck2 = DeckSpecSrc.load deckPath2
      let pl1 = PlayerSpec.create (deck1 |> DeckSpec.name) deck1
      let pl2 = PlayerSpec.create (deck2 |> DeckSpec.name) deck2
      return (pl1, pl2)
    }

  let runGameWithBroadcaster (pl1, pl2) =
    let g = Game.create pl1 pl2
    use o = Broadcaster.subscribe g
    let _ = g |> Game.run
    in ()

  let showGame deckPaths =
    trial {
      let! (pl1, pl2) = loadDecks deckPaths
      do runGameWithBroadcaster (pl1, pl2)
    }

  let usage () =
    """
help                    Print this
show deck1 deck2        Show a battle deck1 vs deck2
"""

  let rec procCommandArgs =
    function
    | [] ->
        match Console.ReadLine() with
        | null | "" -> () |> pass
        | line ->
            line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
            |> procCommandArgs

    | ["show"] ->
        procCommandArgs ["show"; "l.tritrain_deck"; "r.tritrain_deck"]
    | "show" :: deckPath1 :: deckPath2 :: _ ->
        showGame (deckPath1, deckPath2)

    | _ ->
        printfn "%s" (usage ()) |> pass

  [<EntryPoint>]
  let main argv =
    procCommandArgs (argv |> Array.toList)
    |> Trial.runConsoleApp
