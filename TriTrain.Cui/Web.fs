module TriTrain.Cui.Web

open System
open System.IO
open System.Text
open System.Net
open System.Web // references System.Web
open TriTrain.Core.Util // tap

module Map =
  let toFormUrlEncoded (m: Map<string, string>) =
    m |> Map.toList
    |> List.map (fun (k, v) ->
        HttpUtility.UrlEncode(k) + "=" + HttpUtility.UrlEncode(v)
        )
    |> String.concat "&"
    |> Encoding.UTF8.GetBytes

module HttpWebRequest =
  let private createWithCookie (url: string) method' cookie =
    HttpWebRequest.CreateHttp(url)
    |> tap (fun req ->
        req.CookieContainer   <- cookie
        req.Method            <- method'
        )

  let createGet url accept cookie =
    createWithCookie url (WebRequestMethods.Http.Get) cookie
    |> tap (fun req ->
        req.Accept <- accept
        )

  let createPostAsync url args cookie =
    async {
      let content = args |> Map.toFormUrlEncoded
      let req     = createWithCookie url (WebRequestMethods.Http.Post) cookie
      req.ContentLength     <- content.LongLength
      req.ContentType       <- "application/x-www-form-urlencoded"
      let! stream = req.GetRequestStreamAsync() |> Async.AwaitTask
      do! stream.AsyncWrite(content)
      return req
    }

  let getResponseAsync (req: HttpWebRequest) =
    req.GetResponseAsync() |> Async.AwaitTask
