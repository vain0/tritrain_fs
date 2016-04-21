namespace TriTrain.Cui

open TriTrain.Core
open System
open Chessie.ErrorHandling

module Cake =
  let url = @"http://vain0.s2.xrea.com/tritrain_cake/services"

  let (|Command|_|) =
    function
    | "cake" :: _ ->
        trial {
          return ()
        } |> Some
    | _ -> None
