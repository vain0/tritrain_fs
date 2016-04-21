namespace TriTrain.Cui

open TriTrain.Core
open System
open System.Net
open Chessie.ErrorHandling
open FSharp.Data

module Cake =
  let url = @"http://vain0.s2.xrea.com/tritrain_cake/services"

  let requestJsonAsync args =
    async {
      let url = url + "/" + (args |> String.concat "/")
      let req = WebRequest.CreateHttp(url)
      req.Method <- WebRequestMethods.Http.Get
      req.Accept <- "application/json"
      return! req.GetResponseAsync() |> Async.AwaitTask
    }

  let (|Command|_|) =
    function
    | "cake" :: _ ->
        trial {
          return ()
        } |> Some
    | _ -> None
