namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Core.Serialize
open System
open Chessie.ErrorHandling

module Random =
  let (|Random|_|) =
    function
    | "rand" :: _ ->
        trial {
          return
            Random.deckSpec ()
            |> DeckSpec.toSrc 
            |> DeckSpecSrc.serialize
            |> printfn "%s"
        } |> Some
    | _ -> None
