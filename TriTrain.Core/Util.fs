namespace TriTrain.Core

open System
open Printf
open Chessie.ErrorHandling

[<AutoOpen>]
module Misc =
  let tap f x = f x; x

  let flip f x y = f y x

  let isInInterval l r x =
    l <= x && x < r

  type T3<'t> = 't * 't * 't
  type T7<'t> = 't * 't * 't * 't * 't * 't * 't

  type Rate = float

[<RequireQualifiedAccess>]
module T3 =
  let toList (x0, x1, x2) =
    [x0; x1; x2]

  let ofList =
    function
    | [x0; x1; x2] ->
        (x0, x1, x2) |> Some
    | _ -> None

[<AutoOpen>]
module T7 =
  let toList (x0, x1, x2, x3, x4, x5, x6) =
    [x0; x1; x2; x3; x4; x5; x6]

  let ofList =
    function
    | [x0; x1; x2; x3; x4; x5; x6] ->
        (x0, x1, x2, x3, x4, x5, x6) |> Some
    | _ -> None

[<RequireQualifiedAccess>]
module Seq =
  let product xs ys =
    seq {
      for x in xs do
      for y in ys -> (x, y) }

[<RequireQualifiedAccess>]
module Option =
  let getOr x =
    function
    | Some x -> x
    | None -> x

[<RequireQualifiedAccess>]
module List =
  /// List.zip を行う。
  /// 長さが異なる場合は、短いほうに合わせて縮める。
  let zipShrink l r =
    let len = min (l |> List.length) (r |> List.length)
    let l = l |> List.take len
    let r = r |> List.take len
    in List.zip l r

  let tryMaxBy proj xs =
    xs
    |> List.fold (fun ma x ->
        let projX = proj x
        let ma' =
          match ma with
          | Some (max', projMax) when projMax >= projX -> ma
          | _ -> Some (x, projX)
        in ma'
        ) None
    |> Option.map fst

  /// Apply f for each element in xs and partition them into two list.
  /// The fst is y's where f x = Some y
  /// and the other is x's where f x = None.
  let paritionMap (f: 'x -> option<'y>) (xs: list<'x>): (list<'y> * list<'x>) =
    xs
    |> List.fold (fun (l, r) x ->
        match f x with
        | Some y -> (y :: l, r)
        | None -> (l, x :: r)
        ) ([], [])
    |> (fun (l, r) -> (l |> List.rev, r |> List.rev))

[<RequireQualifiedAccess>]
module Map =
  let singleton k v =
    Map.ofList [(k, v)]

  let append l r =
    r |> Map.fold (fun l k v -> l |> Map.add k v) l

  let keySet (m: Map<'k, 'v>): Set<'k> =
    m |> Map.toList |> List.map fst |> Set.ofList

  let valueSet (m: Map<'k, 'v>): Set<'v> =
    m |> Map.toList |> List.map snd |> Set.ofList

  let pullBack value m =
    m
    |> Map.toList
    |> List.choose (fun (k, v) ->
        if v = value then Some k else None
        )
    |> Set.ofList

  let choose f m =
    m |> Map.fold (fun m k v ->
        match f k v with
        | None      -> m
        | Some v'   -> m |> Map.add k v'
        ) Map.empty

[<RequireQualifiedAccess>]
module String =
  let isNamey =
    let acceptableChar ch =
      Char.IsLetter(ch)
      || Char.IsDigit(ch)
      || Char.IsWhiteSpace(ch)
      || (ch = '_')
    let body s =
      s |> String.forall acceptableChar
    in body

module Reflection =
  open Microsoft.FSharp.Reflection

  type DU<'t>() =
    static member val CaseInfos =
      FSharpType.GetUnionCases(typeof<'t>)
      |> Array.toList

    static member val Names =
      DU<'t>.CaseInfos
      |> List.map (fun (case: UnionCaseInfo) -> case.Name)

    static member val UnitCases =
      DU<'t>.CaseInfos
      |> List.choose (fun ci ->
          if ci.GetFields().Length = 0
          then Some (FSharpValue.MakeUnion(ci, Array.empty) :?> 't)
          else None
          )

    static member TryParse(str) =
      DU<'t>.CaseInfos
      |> List.tryFind (fun case -> case.Name = str)
      |> Option.map (fun case -> FSharpValue.MakeUnion (case, [||]) :?> 't)

[<RequireQualifiedAccess>]
module Random =
  let rng = Random()

  let roll (prob: float) =
    rng.NextDouble() < prob
    || prob >= 100.0

[<RequireQualifiedAccess>]
module Observable =
  open System.Diagnostics

  let indexed obs =
    obs
    |> Observable.scan
        (fun (opt, i) x -> (Some x, i + 1)) (None, -1)
    |> Observable.choose
        (fun (opt, i) -> opt |> Option.map (fun x -> (x, i)))

  let duplicateFirst obs =
    let obs' =
      obs
      |> indexed
      |> Observable.choose
          (fun (x, i) -> if i = 0 then Some x else None)
    in Observable.merge obs obs'

  type Source<'t>() =
    let protect f =
      let mutable ok = false
      try 
        f ()
        ok <- true
      finally
        Debug.Assert(ok, "IObserver method threw an exception.")

    let mutable key = 0
    let mutable subscriptions = (Map.empty: Map<int, IObserver<'t>>)

    let thisLock = new obj()

    let subscribe obs =
      let body () =
        key |> tap (fun k ->
          do key <- k + 1
          do subscriptions <- subscriptions |> Map.add k obs
          )
      in lock thisLock body

    let unsubscribe k =
      let body () =
        subscriptions <- subscriptions |> Map.remove k
      in
        lock thisLock body

    let next obs =
      subscriptions |> Map.iter (fun _ value ->
        protect (fun () -> value.OnNext(obs)))

    let completed () =
      subscriptions |> Map.iter (fun _ value ->
        protect (fun () -> value.OnCompleted()))

    let error err =
      subscriptions |> Map.iter (fun _ value ->
        protect (fun () -> value.OnError(err)))

    let obs = 
      { new IObservable<'t> with
          member this.Subscribe(obs) =
            let cancelKey = subscribe obs
            { new IDisposable with 
                member this.Dispose() = unsubscribe cancelKey
                }
          }

    let mutable finished = false

    member this.Next(obs) =
      Debug.Assert(not finished, "IObserver is already finished")
      next obs

    member this.Completed() =
      Debug.Assert(not finished, "IObserver is already finished")
      finished <- true
      completed()

    member this.Error(err) =
      Debug.Assert(not finished, "IObserver is already finished")
      finished <- true
      error err

    member this.AsObservable = obs

[<AutoOpen>]
module TrialOperators =
  let failf fmt =
    kprintf fail fmt

module Yaml =
  open FsYaml

  let myTryLoad<'t> yaml =
    try
      Yaml.load<'t> yaml
      |> pass
    with
    | e -> e |> fail
