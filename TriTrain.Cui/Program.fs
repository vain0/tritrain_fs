namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Core.Serialize
open Chessie.ErrorHandling

module Program =
  let loadDefaultPlayers =
      trial {
        let! deck1 = DeckSpec.load "deck1.yaml"
        let! deck2 = DeckSpec.load "deck2.yaml"
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

  let showErrorMessages r =
    let eprintAll = List.iter (eprintfn "%s")
    match r with
    | Pass _ -> ()
    | Warn (_, msgs) ->
        eprintfn "Warning:"
        eprintAll msgs
    | Fail msgs ->
        eprintfn "Fatal error:"
        eprintAll msgs

  let testPlay () =
    trial {
      let! (pl1, pl2) = loadDefaultPlayers
      do runGameWithBroadcaster (pl1, pl2)
    }
    |> showErrorMessages

  [<EntryPoint>]
  let main argv =
    testPlay ()

    // exit code
    0
