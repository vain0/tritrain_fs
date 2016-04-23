namespace TriTrain.Cui

open TriTrain.Core
open System
open System.Collections.Specialized
open System.Net
open System.IO
open System.Text
open Chessie.ErrorHandling

module NameValueCollection =
  let ofSeq kvs =
    NameValueCollection()
    |> tap (fun nvc ->
        for (k, v) in kvs do
          nvc.Add(k, v)
        )

module Cake =
  let url action =
    @"http://vain0.s2.xrea.com/tritrain_cake/services/"
    + (action |> String.concat "/")

  let getJsonTextAsync action =
    async {
      let req = WebRequest.CreateHttp(url action)
      req.Method <- WebRequestMethods.Http.Get
      req.Accept <- "application/json"
      let! res    = req.GetResponseAsync() |> Async.AwaitTask
      let stream  = res.GetResponseStream()
      let reader  = new StreamReader(stream)
      return! reader.ReadToEndAsync() |> Async.AwaitTask
    }

  let postAsync action data =
    async {
      use wc    = new WebClient()
      let uri   = Uri(url action)
      let data  = NameValueCollection.ofSeq data
      let! buf  =
        wc.UploadValuesTaskAsync(uri, data)
        |> Async.AwaitTask
      return Encoding.UTF8.GetString(buf)
    }

  let tryLogin userName password =
    trial {
      let res = 
        [
          ("username", userName)
          ("password", password)
        ]
        |> postAsync ["login"]
        |> Async.RunSynchronously
      match res |> Int32.TryParse with
      | true, userId -> return userId
      | false, _ -> return! fail "User name or password is incorrect."
    }

  let logout () =
    []
    |> postAsync ["logout"]
    |> Async.Ignore
    |> Async.RunSynchronously

  let usage () =
    """
Type one of these commands:
(No commands available now)
"""

  let rec cake () =
    trial {
      match Console.ReadLine() with
      | null | "end" | "exit" | "halt" | "quit" -> return ()
      | line ->
          let args = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
          match args |> Array.toList with
          | _ -> printfn "%s" (usage ())
          return! cake ()
    }

  let (|Command|_|) =
    function
    | "json" :: args ->
        trial {
          getJsonTextAsync args
          |> Async.RunSynchronously
          |> printfn "%s"
        } |> Some
    | "cake" :: userName :: password :: _ ->
        trial {
          let! userId = tryLogin userName password
          try
            printfn "Welcome!"
            do! cake ()
          finally
            logout ()
        } |> Some
    | _ -> None
