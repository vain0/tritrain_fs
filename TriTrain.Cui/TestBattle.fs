namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Core.Serialize
open TriTrain.Core.TestBattle
open System
open Chessie.ErrorHandling

module TestBattle =
  let defaultDeckPaths =
    ["l.trtrdeck"; "r.trtrdeck"]

  let loadDecks (deckPath1, deckPath2) =
    trial {
      let! deck1 = DeckSpecSrc.load deckPath1
      let! deck2 = DeckSpecSrc.load deckPath2
      let pl1 = PlayerSpec.create (deck1 |> DeckSpec.name) deck1
      let pl2 = PlayerSpec.create (deck2 |> DeckSpec.name) deck2
      return (pl1, pl2)
    }

  let showGame deckPaths =
    trial {
      let! plPair = loadDecks deckPaths
      let _ = runGameWithObserver (Broadcaster.observe) plPair
      in ()
    }

  let stringizeResult (win, lose, draw) =
    sprintf "Win %d - Lose %d - Draw %d" win lose draw

  let testBattleCommand deckPaths =
    trial {
      let! plPair = loadDecks deckPaths
      let result = testBattle 10 plPair
      let () = printfn "%s" (stringizeResult result)
      in ()
    }

  // 総当たりの結果をリスト形式で表示する
  let printRoundRobinResultsAsList results =
    for ((pl1, pl2), result) in results do
      printfn "%s vs %s: %s"
        (pl1 |> PlayerSpec.name)
        (pl2 |> PlayerSpec.name)
        (stringizeResult result)

  let roundRobinCommand deckPaths =
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
    }
