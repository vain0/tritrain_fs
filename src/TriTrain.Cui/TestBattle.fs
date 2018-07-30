namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Core.Serialize
open TriTrain.Core.TestBattle
open System
open Chessie.ErrorHandling

module TestBattle =
  let defaultDeckPaths =
    ("l.trtrdeck", "r.trtrdeck")

  /// Accept two deck paths or use default deck paths
  let (|DeckPathPair|) =
    function
    | deck1 :: deck2 :: rest ->
        ((deck1, deck2), rest)
    | rest ->
        (defaultDeckPaths, rest)

  let (|DeckPathList|_|) =
    function
    | [] -> defaultDeckPaths |> T2.toList |> Some
    | [_] -> None
    | (_ :: _ :: _) as deckPathList -> deckPathList |> Some

  let loadDecks (deckPath1, deckPath2) =
    trial {
      let! deck1 = DeckSpecSrc.load deckPath1
      let! deck2 = DeckSpecSrc.load deckPath2
      let pl1 = PlayerSpec.create (deck1 |> DeckSpec.name) deck1
      let pl2 = PlayerSpec.create (deck2 |> DeckSpec.name) deck2
      return (pl1, pl2)
    }

  let (|ShowGame|_|) =
    function
    | "show" :: DeckPathPair (deckPaths, _) ->
        trial {
          let! plPair = loadDecks deckPaths
          let _ =
            runGameWithObserver
              (Broadcaster.observe (* paginates = *) true)
              plPair
          in ()
        } |> Some
    | _ -> None

  let stringizeResult (win, lose, draw) =
    sprintf "Win %d - Lose %d - Draw %d" win lose draw

  let (|TestBattle|_|) =
    function
    | "test" :: DeckPathPair (deckPaths, _) ->
        trial {
          let! plPair = loadDecks deckPaths
          let result = testBattle 10 plPair
          let () = printfn "%s" (stringizeResult result)
          in ()
        } |> Some
    | _ -> None

  // 総当たりの結果をリスト形式で表示する
  let printRoundRobinResultsAsList results =
    for ((pl1, pl2), result) in results do
      printfn "%s vs %s: %s"
        (pl1 |> PlayerSpec.name)
        (pl2 |> PlayerSpec.name)
        (stringizeResult result)

  let (|RoundRobin|_|) =
    function
    | ("rr" | "round-robin") :: DeckPathList deckPaths ->
        trial {
          let! decks =
            deckPaths
            |> List.map (DeckSpecSrc.load)
            |> Trial.collect
          let pls =
            [ for deck in decks ->
                PlayerSpec.create (deck |> DeckSpec.name) deck ]
          let results =
            roundRobin 10 pls
          do printRoundRobinResultsAsList results
        } |> Some
    | _ -> None
