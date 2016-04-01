namespace TriTrain.Core

open System

[<AutoOpen>]
module Misc =
  let tap f x = f x; x

  let flip f x y = f y x

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
module Random =
  let rng = Random()

  let roll (prob: float) =
    rng.NextDouble() < prob
    || prob >= 100.0
