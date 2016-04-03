namespace TriTrain.Cui

open System
open TriTrain.Core

module Broadcaster =
  let stringizeSide =
    function
    | PlLft -> "左陣"
    | PlRgt -> "右陣"

  let stringizeVertex =
    function
    | Fwd -> "前"
    | Lft -> "左"
    | Rgt -> "右"

  let stringizePlace (plId, vx) =
    sprintf "[%s%s]"
      (stringizeSide plId)
      (stringizeVertex vx)

  let printCardName g cardId =
    let card    = g |> Game.card cardId
    let name    = card |> Card.name
    let st      = card |> Card.curStatus
    in
      match g |> Game.searchBoardFor cardId with
      | Some place ->
          printf "%s%s[%d/%d/%d]"
            name
            (stringizePlace place)
            (st |> Status.hp)
            (st |> Status.at)
            (st |> Status.ag)
      | None ->
          printf "《%s》"  name

  let printEvent (ev, g, g') =
    match ev with
    | GameBegin ->
        printfn "-------- Game Begin --------"
        printfn "%s vs %s"
          (g |> Game.plLft |> Player.name)
          (g |> Game.plRgt |> Player.name)

    | GameEnd Draw ->
        printfn "Draw game."

    | GameEnd (Win plId) ->
        printfn "%s win!"
          (g' |> Game.player plId |> Player.name)

    | TurnBegin ->
        printfn "---- Turn %d ----"
          (g' |> Game.turn)

    | CardEnter (cardId, _) ->
        printCardName g' cardId
        printfn " is summoned!"

    | CardActBegin (cardId, (name, _)) ->
        printCardName g' cardId
        printfn " does '%s'!"
          name

    | CardHpInc (cardId, amount) ->
        let curHp = Game.card cardId >> Card.curHp
        do
          printCardName g' cardId
          printfn "'s HP%d→%d (%+d)"
            (g  |> curHp)
            (g' |> curHp)
            amount

    | CardDie cardId ->
        printCardName g cardId  // 死亡する直前の状態を表示
        printfn " died."

    | CardGainEffect (cardId, keff) ->
        printCardName g cardId      // 獲得前の状態を表示
        printfn " gains %A."        // TODO: 分かりやすく表示する
          keff

    | CardLoseEffect (cardId, keff) ->
        printCardName g' cardId
        printfn " lost %A."
          keff

    | CardMove moves ->
        for (cardId, _, dst) in moves do
          printCardName g cardId  // 移動前の位置を表示する
          printfn " moves to %s."
            (stringizePlace dst)

  let subscribe g: IDisposable =
    g
    |> Game.asObservable
    |> Observable.subscribe printEvent
