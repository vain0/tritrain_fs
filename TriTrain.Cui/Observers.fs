namespace TriTrain.Cui

open System
open TriTrain.Core
open Microsoft.FSharp.Control

module Broadcaster =
  let colorFromPlayerId =
    function
    | PlLft -> ConsoleColor.Green
    | PlRgt -> ConsoleColor.Red

  let cprintf  plId = Console.mcprintf  id (plId |> colorFromPlayerId)
  let cprintfn plId = Console.mcprintfn id (plId |> colorFromPlayerId)

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
    let st      = card |> Card.status
    let owner   = cardId |> CardId.owner
    in
      match g |> Game.searchBoardFor cardId with
      | Some place ->
          cprintf owner
            "%s%s[%d/%d/%d]"
            name
            (stringizePlace place)
            (st |> Status.hp)
            (st |> Status.at)
            (st |> Status.ag)
      | None ->
          cprintf owner
            "《%s》" name

  let printPlayerName g plId =
    cprintf plId "%s" (g |> Game.player plId |> Player.name)

  let printEvent (ev, g, g') =
    match ev with
    | GameBegin ->
        printfn "-------- Game Begin --------"
        printPlayerName g' PlLft
        printf " vs "
        printPlayerName g' PlRgt
        printfn ""

    | GameEnd Draw ->
        printfn "Draw game."

    | GameEnd (Win plId) ->
        printPlayerName g' plId
        printfn " won!"

    | TurnBegin ->
        printfn "---- Turn %d ----"
          (g' |> Game.turn)

        /// Wait input
        Console.ReadLine() |> ignore

    | WindBlow ->
        printfn "--------"
        printfn "The wind blows."

    | CardEnter (cardId, _) ->
        printCardName g' cardId
        printfn " is summoned!"

    | CardBeginAction (cardId, skill) ->
        printfn "--------"
        printCardName g' cardId
        printfn " does '%s'!"
          (skill |> Skill.name)

    | CardAbilityTrigger (cardId, _, (name, _)) ->
        printCardName g' cardId
        printfn "'s '%s' triggered!"
          name

    | SolveTriggered (cardId, _, (name, _)) ->
        printfn "----"
        printCardName g' cardId
        printfn "'s '%s' is solved."
          name

    | CardNullifyEffect (cardId, _) ->
        printCardName g' cardId
        printfn " nullified an effect."

    | CardHpInc (cardId, amount) ->
        let hp = Game.card cardId >> Card.hp
        do
          printCardName g' cardId
          printfn "'s HP%d→%d (%+d)"
            (g  |> hp)
            (g' |> hp)
            amount

    | CardRegenerate (cardId, amount) ->
        printCardName g' cardId
        printfn " died and regenerated."

    | CardDie cardId ->
        printCardName g cardId  // 死亡する直前の状態を表示
        printfn " died."

    | CardGainEffect (cardId, keff) ->
        printCardName g cardId      // 獲得前の状態を表示
        printfn " gains %s."        // TODO: 分かりやすく表示する
          (Dump.dumpKEffect keff)

    | CardLoseEffect (cardId, keff) ->
        printCardName g' cardId
        printfn " lost %s."
          (Dump.dumpKEffect keff)

    | CardMove moves ->
        for (cardId, _, dst) in moves do
          printCardName g cardId  // 移動前の位置を表示する
          printfn " moves to %s."
            (stringizePlace dst)

  let observe g: IDisposable =
    g
    |> Game.asObservable
    |> Observable.duplicateFirst
    |> Observable.pairwise
    |> Observable.map (fun ((_, g), (ev, g')) -> (ev, g, g'))
    |> Observable.subscribe printEvent
