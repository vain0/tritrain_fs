namespace TriTrain.Cui

open TriTrain.Core
open System
open System.Net
open System.IO
open Chessie.ErrorHandling

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

  let requestJsonTextAsync args =
    async {
      let! res    = requestJsonAsync args
      let stream  = res.GetResponseStream()
      let reader  = new StreamReader(stream)
      return! reader.ReadToEndAsync() |> Async.AwaitTask
    }

  let (|Command|_|) =
    function
    | "json" :: args ->
        trial {
          requestJsonTextAsync args
          |> Async.RunSynchronously
          |> printfn "%s"
        } |> Some
    | _ -> None
