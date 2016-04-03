namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Core.Serialize
open Chessie.ErrorHandling

module Program =
  let loadDefaultPlayers =
      trial {
        let! deck1 = DeckSpecSrc.load "l.tritrain_deck"
        let! deck2 = DeckSpecSrc.load "r.tritrain_deck"
        let pl1 =
          {
            Name      = "左人"
            Deck      = deck1
          }
        let pl2 =
          {
            Name      = "右人"
            Deck      = deck2
          }
        return (pl1, pl2)
      }

  let runGameWithBroadcaster (pl1, pl2) =
    let g = Game.create pl1 pl2
    use o = Broadcaster.subscribe g
    let _ = g |> Game.run
    in ()

  let testPlay () =
    trial {
      let! (pl1, pl2) = loadDefaultPlayers
      do runGameWithBroadcaster (pl1, pl2)
    }
    |> Trial.eprintMessages

  [<EntryPoint>]
  let main argv =
    testPlay ()

    // exit code
    0
