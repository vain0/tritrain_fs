namespace TriTrain.Core

open System
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

module ObjectElementSeq =
  open System
  open System.Linq
  open Microsoft.FSharp.Reflection

  let cast (t: Type) (xs: obj seq) =
    let enumerable = typeof<Enumerable>
    let cast =
      let nonGeneric = enumerable.GetMethod("Cast")
      nonGeneric.MakeGenericMethod([| t |])
    cast.Invoke(null, [| xs |])

  let toSet (t: Type) (xs: obj seq) =
    let setType         = typedefof<Set<_>>.MakeGenericType(t)
    let parameter       = xs |> cast t
    let parameterType   = typedefof<seq<_>>.MakeGenericType([| t |])
    let constructor'    = setType.GetConstructor([| parameterType |])
    in constructor'.Invoke([| parameter |])

module Yaml =
  open FsYaml
  open FsYaml.NativeTypes
  open FsYaml.RepresentationTypes
  open FsYaml.CustomTypeDefinition

  let setDef =
    {
      Accept = isGenericTypeDef (typedefof<Set<_>>)
      Construct = fun construct' t ->
        function
        | Sequence (s, _) ->
            let elemType = t.GetGenericArguments().[0]
            let elems = s |> List.map (construct' elemType)
            in ObjectElementSeq.toSet elemType elems
        | otherwise -> raise (mustBeSequence t otherwise)
      Represent =
        representSeqAsSequence
    }

  let customDefs =
    [
      setDef
    ]

  let customDump<'t> x =
    Yaml.dumpWith<'t> customDefs x

  let customTryLoad<'t> yaml =
    try
      Yaml.loadWith<'t> customDefs yaml
      |> pass
    with
    | e -> e |> fail
