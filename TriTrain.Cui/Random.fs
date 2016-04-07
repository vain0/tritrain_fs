namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Core.Serialize
open Chessie.ErrorHandling

module Random =
  let (|RandomCommand|_|): list<string> -> option<Result<unit, string>> =
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
