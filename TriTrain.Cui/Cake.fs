namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Core.Serialize
open TriTrain.Cui.Web
open System
open System.Net
open System.IO
open System.Text
open System.Xml.Linq
open Chessie.ErrorHandling
open Newtonsoft.Json
open Newtonsoft.Json.FSharp
open FsYaml

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
      return! res |> WebResponse.readContentAsync
    }

  let getJsonAsXDocumentAsync action =
    async {
      let! json = getJsonTextAsync action
      return JsonConvert.DeserializeXNode(json)
    }

  let postAsync action args =
    async {
      let! req = HttpWebRequest.createPostAsync (url action) args cookie
      let! res = req |> HttpWebRequest.getResponseAsync
      return! res |> WebResponse.readContentAsync
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

  let join leagueId cardListPath =
    trial {
      let  text      = File.ReadAllText(cardListPath)
      let! cardList  =
        text
        |> Yaml.myTryLoad<CardSpecSrc []>
        |> Trial.mapFailure (List.map (fun e -> e.Message))
      let yamlList   =
        cardList |> Array.map (fun card ->
          [ ("hash", card |> CardSpecSrc.toHash)
            ("yaml", card |> Yaml.dump) ]
          |> Map.ofList
          )
      let res =
        [
          ("cards", JsonConvert.SerializeObject(yamlList))
        ]
        |> Map.ofList
        |> postAsync ["join"; leagueId]
        |> Async.RunSynchronously
      printfn "%s" res
    }

  /// Returns the deck's hash
  let addDeck deckPath =
    trial {
      let! deck = DeckSpecSrc.load deckPath
      let cards =
        deck.Cards |> T7.toList
        |> List.map (CardSpec.toSrc >> CardSpecSrc.toHash)
      let hash  =
        deck |> DeckSpec.toSrc |> DeckSpecSrc.toHash
      do
        [ ("name", deck.Name)
          ("hash", hash)
          ("card_hashes", JsonConvert.SerializeObject(cards))
        ]
        |> Map.ofList
        |> postAsync ["addDeck"]
        |> Async.Ignore
        |> Async.RunSynchronously
      return hash
    }

  let updateNextLeagueDeck leagueId deckPath =
    trial {
      let! hash = deckPath |> addDeck
      do
        Map.empty
        |> postAsync ["updateLeagueNextDeck"; leagueId; hash]
        |> Async.RunSynchronously
        |> printfn "%s"
    }

  let usage () =
    """
Type one of these commands:
join leagueId cardListPath
deck leagueId deckPath          Set the deck for your next game
"""

  let rec cake () =
    trial {
      match Console.ReadLine() with
      | null | "end" | "exit" | "halt" | "quit" -> return ()
      | line ->
          let args = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
          match args |> Array.toList with
          | "join" :: leagueId :: cardListPath :: _ ->
              do! join leagueId cardListPath
          | "deck" :: leagueId :: deckPath :: _ ->
              do! updateNextLeagueDeck leagueId deckPath
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
