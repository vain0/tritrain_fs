namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Cui.Web
open System
open System.Net
open System.IO
open System.Text
open Chessie.ErrorHandling

module Cake =
  let url action =
    @"http://vain0.s2.xrea.com/tritrain_cake/services/"
    + (action |> String.concat "/")

  let cookie = CookieContainer()

  let getJsonTextAsync action =
    async {
      let! res =
        HttpWebRequest.createGet (url action) "application/json" cookie
        |> HttpWebRequest.getResponseAsync 
      return! res.GetResponseStream() |> Stream.readToEndAsync
    }

  let postAsync action args =
    async {
      let! req = HttpWebRequest.createPostAsync (url action) args cookie
      let! res = req |> HttpWebRequest.getResponseAsync
      return! res.GetResponseStream() |> Stream.readToEndAsync
    }

  let tryLogin userName password =
    trial {
      let res = 
        [
          ("username", userName)
          ("password", password)
        ]
        |> Map.ofList
        |> postAsync ["login"]
        |> Async.RunSynchronously

      match res |> Int32.TryParse with
      | true, userId -> return userId
      | false, _ -> return! fail "User name or password is incorrect."
    }

  let logout () =
    Map.empty
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
