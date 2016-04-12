[<AutoOpen>]
module TriTrain.Core.Util

open System
open Printf
open Chessie.ErrorHandling

[<AutoOpen>]
module Misc =
  let tap (f: 'x -> unit) (x: 'x): 'x = f x; x

  let flip f x y = f y x

  let isInInterval l r x: bool =
    l <= x && x < r

  let fold' (xs: #seq<'x>) (f: 'x -> 's -> 's) (s: 's): 's =
    xs |> Seq.fold (flip f) s 

  type T7<'t> = 't * 't * 't * 't * 't * 't * 't

  type Rate = float

[<RequireQualifiedAccess>]
module T2 =
  let toList (x0, x1): list<_> =
    [x0; x1]

[<RequireQualifiedAccess>]
module T7 =
  let toList (x0, x1, x2, x3, x4, x5, x6): list<_> =
    [x0; x1; x2; x3; x4; x5; x6]

  let ofList: list<'t> -> option<T7<'t>> =
    function
    | [x0; x1; x2; x3; x4; x5; x6] ->
        (x0, x1, x2, x3, x4, x5, x6) |> Some
    | _ -> None

  let init (f: int -> 't): T7<'t> =
    (f 0, f 1, f 2, f 3, f 4, f 5, f 6)

  let map (f: 'x -> 'y) ((x0, x1, x2, x3, x4, x5, x6): T7<'x>): T7<'y> =
    (f x0, f x1, f x2, f x3, f x4, f x5, f x6)

[<RequireQualifiedAccess>]
module Seq =
  let item i xs =
    xs |> Seq.skip i |> Seq.head

  let product (xs: seq<'x>) (ys: seq<'y>): seq<'x * 'y> =
    seq {
      for x in xs do
      for y in ys -> (x, y) }

[<RequireQualifiedAccess>]
module Option =
  let getOr (x: 'x): option<'x> -> 'x =
    function
    | Some x -> x
    | None -> x

[<RequireQualifiedAccess>]
module List =
  let take len xs =
    xs |> List.toSeq |> Seq.take len |> List.ofSeq

  /// List.zip を行う。
  /// 長さが異なる場合は、短いほうに合わせて縮める。
  let zipShrink (l: list<'l>) (r: list<'r>): list<'l * 'r> =
    let len = min (l |> List.length) (r |> List.length)
    let l = l |> take len
    let r = r |> take len
    in List.zip l r

  let tryMaxBy (proj: 'x -> 'y) (xs: list<'x>): option<'x> =
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
  let singleton (k: 'k) (v: 'v): Map<'k, 'v> =
    Map.ofList [(k, v)]

  let append (l: Map<'k, 'v>) (r: Map<'k, 'v>): Map<'k, 'v> =
    r |> Map.fold (fun l k v -> l |> Map.add k v) l

  let keySet (m: Map<'k, 'v>): Set<'k> =
    m |> Map.toList |> List.map fst |> Set.ofList

  let valueList (m: Map<'k, 'v>): list<'v> =
    m |> Map.toList |> List.map snd

  let valueSet (m: Map<'k, 'v>): Set<'v> =
    m |> valueList |> Set.ofList

  let pullBack (value: 'v) (m: Map<'k, 'v>): Set<'k> =
    m
    |> Map.toList
    |> List.choose (fun (k, v) ->
        if v = value then Some k else None
        )
    |> Set.ofList

  let choose (f: 'k -> 'v -> option<'w>) (m: Map<'k, 'v>): Map<'k, 'w> =
    m |> Map.fold (fun m k v ->
        match f k v with
        | None      -> m
        | Some v'   -> m |> Map.add k v'
        ) Map.empty

  /// The number of key-value pairs
  let size self: int =
    self |> Map.toSeq |> Seq.length

module Reflection =
  open Microsoft.FSharp.Reflection

  type DU<'t when 't: comparison>() =
    static member val CaseInfos =
      FSharpType.GetUnionCases(typeof<'t>)
      |> Array.toList

    static member val Names: list<string> =
      DU<'t>.CaseInfos
      |> List.map (fun (case: UnionCaseInfo) -> case.Name)

    static member TryParse(str: string): option<'t> =
      DU<'t>.CaseInfos
      |> List.tryFind (fun case -> case.Name = str)
      |> Option.map (fun case -> FSharpValue.MakeUnion (case, [||]) :?> 't)

    static member val internal StringizeUnitCaseMap: Map<'t, string> =
      [ for ci in DU<'t>.CaseInfos do
          if ci.GetFields().Length = 0 then
            yield (FSharpValue.MakeUnion(ci, Array.empty) :?> 't, ci.Name)
      ] |> Map.ofList

    static member StringizeUnitCase(case: 't): string =
      DU<'t>.StringizeUnitCaseMap |> Map.find case

    static member val UnitCases: list<'t> =
      DU<'t>.StringizeUnitCaseMap |> Map.toList |> List.map fst

[<RequireQualifiedAccess>]
module Random =
  let rng = Random()

  let roll (prob: float) =
    rng.NextDouble() < prob
    || prob >= 100.0

  let element (xs: seq<'x>): option<'x> =
    let len = xs |> Seq.length
    in
      if len = 0
      then None
      else xs |> Seq.item (rng.Next(0, len - 1)) |> Some

[<RequireQualifiedAccess>]
module Observable =
  open System.Diagnostics

  let subscribeAll
      (onNext: 't -> unit)
      (onError: exn -> unit)
      (onCompleted: unit -> unit)
      (obs: IObservable<'t>)
      : IDisposable
    =
    let observer =
      { new IObserver<'t> with
          member this.OnNext(x) = onNext x
          member this.OnError(e) = onError e
          member this.OnCompleted() = onCompleted ()
      }
    in obs.Subscribe(observer)

  let indexed (obs: IObservable<'x>): IObservable<'x * int> =
    obs
    |> Observable.scan
        (fun (opt, i) x -> (Some x, i + 1)) (None, -1)
    |> Observable.choose
        (fun (opt, i) -> opt |> Option.map (fun x -> (x, i)))

  let duplicateFirst (obs: IObservable<'x>): IObservable<'x> =
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

    member this.AsObservable: IObservable<'t> = obs

type BatchedQueue<'T> =
  | BatchedQueue of forwardList: list<'T> * reversedList: list<'T>

[<RequireQualifiedAccess>]
module BatchedQueue =
  let internal unwrap (BatchedQueue(l, r)) =
    (l, r)

  // 前方リストが空なら、後方リストを反転して前方リストにする
  let internal rebuild self =
    self
    |> unwrap
    |> function
      | ([], r) -> BatchedQueue (r |> List.rev, [])
      | _ -> self

  let empty: BatchedQueue<_> =
    BatchedQueue ([], [])

  let add (x: 'x) (self: BatchedQueue<'x>): BatchedQueue<'x> =
    self
    |> unwrap
    |> function (l, r) -> BatchedQueue (l, x :: r)
    |> rebuild

  let tryUncons (self: BatchedQueue<'x>): option<'x * BatchedQueue<'x>> =
    self
    |> unwrap
    |> function
      | ([], _) -> None
      | (x :: l, r) -> Some (x, BatchedQueue (l, r) |> rebuild)

  let tryHead (self: BatchedQueue<'x>): option<'x> =
    self |> tryUncons |> Option.map fst

  let tryTail (self: BatchedQueue<'x>): option<BatchedQueue<'x>> =
    self |> tryUncons |> Option.map snd

  let toList (self: BatchedQueue<'x>): list<'x> =
    self
    |> unwrap
    |> function (f, r) -> List.append f (List.rev r)

  let ofList (xs: list<'x>): BatchedQueue<'x> =
    BatchedQueue (xs, [])

  let toSeq self = self |> toList |> List.toSeq
  let ofSeq self = self |> Seq.toList |> ofList

  let singleton x =
    ofList [x]

  // Note: キューが空でないときは常に、前方リストが空でない。
  let isEmpty (BatchedQueue (l, _)): bool =
    List.isEmpty l

  let length (self: BatchedQueue<_>): int =
    self
    |> unwrap
    |> function (l, r) -> List.length l + List.length r

  let map (f: 'x -> 'y) (self: BatchedQueue<'x>): BatchedQueue<'y> =
    self
    |> unwrap
    |> function (l, r) -> BatchedQueue (List.map f l, List.map f r)

  let fold (f: 's -> 'x -> 's) (s: 's) (self: BatchedQueue<'x>): 's =
    self |> toList |> List.fold f s

  let append (l: BatchedQueue<'x>) (r: BatchedQueue<'x>): BatchedQueue<'x> =
    r |> fold (fun self x -> self |> add x) l

[<AutoOpen>]
module TrialOperators =
  let warnf value fmt =
    kprintf (flip warn value) fmt

  let failf fmt =
    kprintf fail fmt

  let failfIfNone fmt =
    kprintf failIfNone fmt

module Yaml =
  open FsYaml

  let myTryLoad<'t> (yaml: string): Result<'t, exn> =
    try
      Yaml.load<'t> yaml
      |> pass
    with
    | e -> e |> fail
