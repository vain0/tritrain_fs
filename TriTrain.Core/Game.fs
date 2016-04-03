namespace TriTrain.Core

module Game =
  let plLft       (g: Game) = g.PlLft
  let plRgt       (g: Game) = g.PlRgt
  let cardMap     (g: Game) = g.CardMap
  let turn        (g: Game) = g.Turn

  let create plLftSpec plRgtSpec =
    let (plLft, deckLft) = Player.create plLftSpec PlLft
    let (plRgt, deckRgt) = Player.create plRgtSpec PlRgt
    let cardMap =
      List.append deckLft deckRgt
      |> List.map (fun card -> (card |> Card.cardId, card))
      |> Map.ofList
    in
      {
        PlLft       = plLft
        PlRgt       = plRgt
        CardMap     = cardMap
        Turn        = 0
        Triggered   = []
      }

  let player plId g =
    match plId with
    | PlLft -> g |> plLft
    | PlRgt -> g |> plRgt

  let deck plId g =
    g |> player plId |> Player.deck

  let board plId g =
    g |> player plId |> Player.board

  let trash plId g =
    g |> player plId |> Player.trash

  let card cardId g =
    g |> cardMap |> Map.find cardId  // noexcept

  let updatePlayer pl g =
    match pl |> Player.playerId with
    | PlLft -> { g with PlLft = pl }
    | PlRgt -> { g with PlRgt = pl }

  let updateDeck plId deck' g =
    g |> updatePlayer { (g |> player plId) with Deck = deck' }

  let updateBoard plId board' g =
    g |> updatePlayer { (g |> player plId) with Board = board' }

  let updateTrash plId trash' g =
    g |> updatePlayer { (g |> player plId) with Trash = trash' }

  let updateCard card' g =
    let cardMap' =
      g |> cardMap |> Map.add (card' |> Card.cardId) card'
    in { g with CardMap = cardMap' }

  let placeMap g: Map<Place, CardId> =
    PlayerId.all
    |> List.collect (fun plId ->
        g
        |> board plId
        |> Map.toList
        |> List.map (fun (vx, cardId) -> ((plId, vx), cardId))
        )
    |> Map.ofList

  let searchBoardFor cardId g =
    match g |> placeMap |> Map.pullBack cardId |> Set.toList with
    | [] -> None
    | [place] -> Some place
    | _ -> failwith "impossible"

  let cardIdsOnBoard g =
    g |> placeMap |> Map.valueSet

  let dieCard cardId g =
    match g |> searchBoardFor cardId with
    | None -> g
    | Some (plId, vx) ->
        // TODO: PIG能力が誘発
        let board' = g |> board plId |> Map.remove vx
        let trash' = g |> trash plId |> Set.add cardId
        in
          g
          |> updateBoard plId board'
          |> updateTrash plId trash'

  let incCardHp targetId amount g =
    let target    = g |> card targetId
    let hp'       = target |> Card.curHp |> (+) amount |> max 0 
    let g         = g |> updateCard { target with CurHP = hp' }
    let g =
      if hp' = 0
      then g |> dieCard targetId
      else g
    in g

  let giveKEffectTo targetId keff g =
    let target    = g |> card targetId
    let effs'     = keff :: (target |> Card.effects)
    let g         = g |> updateCard { target with Effects = effs' }
    in g

  let rec procOEffectToUnit actorOpt targetId oeffType g =
    match oeffType with
    | Damage amount ->
        let amount = amount |> Amount.resolve actorOpt |> int |> max 0
        in g |> incCardHp targetId (- amount)

    | Heal amount ->
        let amount = amount |> Amount.resolve actorOpt |> int |> max 0
        in g |> incCardHp targetId amount

    | Death  amount ->
        let prob   = amount |> Amount.resolve actorOpt |> flip (/) 100.0
        let g =
          if Random.roll prob then
            let target  = g |> card targetId
            let amount  = target |> Card.curHp |> (~-)
            let g       = g |> incCardHp targetId amount
            in g
          else g
        in g

    | Giving keff ->
        g |> giveKEffectTo targetId keff

  let rec procOEffect actorOpt (source: Place) oeff g =
    match oeff with
    | OEffectList oeffs ->
        oeffs |> List.fold (flip (procOEffect actorOpt source)) g
    | GenToken cardSpecs ->
        g // TODO: トークン生成
    | Swap scope ->
        match scope |> Scope.placeSet source |> Set.toList with
        | [r1; r2] ->
            g // TODO: 交代
        | _ -> g
    | OEffectToUnits (typ, scope) ->
        let targets =
          scope |> Scope.placeSet source
          |> Set.toList
          |> List.choose (fun (plId, vx) ->
              g |> board plId |> Map.tryFind vx
              )
        let g =
          targets |> List.fold (fun g cardId ->
              g |> procOEffectToUnit actorOpt cardId typ
              ) g
        in g

  /// 未行動な最速カード
  let tryFindFastest actedCards g: option<Vertex * CardId> =
    g
    |> placeMap
    |> Map.toList  // 位置順
    |> List.choose (fun ((_, vx), cardId) ->
        if actedCards |> Set.contains cardId
        then None
        else (g |> card cardId |> Card.curAg, (vx, cardId)) |> Some
        )
    |> List.tryMaxBy fst
    |> Option.map snd

  /// カード actor の行動を処理する
  let procSkill actorId vx g =
    let actor = g |> card actorId
    let skillOpt =
      actor
      |> Card.spec
      |> CardSpec.skills
      |> Map.tryFind (vx |> Row.ofVertex)
    let g =
      match skillOpt with
      | None -> g
      | Some oeff ->
          g |> procOEffect (Some actor) (actorId |> CardId.owner, vx) oeff
    in g

  /// プレイヤー plId が位置 vx にデッキトップを召喚する。
  /// デッキが空なら何もしない。
  let summon plId vx g =
    let deck  = g |> deck plId
    let board = g |> board plId
    let () =
      assert (board |> Map.containsKey vx |> not)
    let g =
      match deck with
      | [] -> g
      | cardId :: deck' ->
          g
          |> updateDeck plId deck'
          |> updateBoard plId
              (board |> Map.add vx cardId)
          // TODO: EtB能力が誘発
    in g

  let procSummonPhase plId g =
    g
    |> board plId
    |> Board.emptyVertexSet
    |> Set.fold (fun g vx -> g |> summon plId vx) g

  /// カードにかかっている継続的効果の経過ターン数を更新する
  let updateDuration cardId g =
    let card = g |> card cardId 
    let effects =
      card
      |> Card.effects
      |> List.choose (fun keff ->
          match keff.Duration with
          | None -> keff |> Some
          | Some n ->
              if n <= 1
              then None
              else { keff with Duration = Some (n - 1) } |> Some
          )
    let card' = { card with Effects = effects }
    in g |> updateCard card'
  
  let updateDurationAll g =
    g
    |> cardIdsOnBoard
    |> Set.fold (fun g cardId ->
        g |> updateDuration cardId
        ) g

  let rec procPhase ph g: Game * GameResult =
    match ph with
    | SummonPhase ->
        let g =
          PlayerId.all
          |> List.fold (fun g plId -> g |> procSummonPhase plId) g
        let isLost plId =
          g |> board plId |> Map.isEmpty
        in
          // 勝敗判定
          match PlayerId.all |> List.filter isLost with
          | []      -> g |> procPhase UpkeepPhase
          | [plId]  -> (g, plId |> PlayerId.inverse |> Win)
          | _       -> (g, Draw)

    | UpkeepPhase ->
        // TODO: BoT能力が誘発
        g |> procPhase RotatePhase

    | ActionPhase actedCards ->
        match g |> tryFindFastest actedCards with
        | Some (vx, actorId) ->
            g
            |> procSkill actorId vx
            |> procPhase (actedCards |> Set.add actorId |> ActionPhase)
        | None ->
            g |> procPhase RotatePhase

    | RotatePhase ->
        PlayerId.all
        |> List.fold (fun g plId ->
            let (board', log) =
              g |> board plId |> Board.rotate
            let g =  // TODO: 移動を通知
              g |> updateBoard plId board'
            in g
            ) g
        |> procPhase PassPhase

    | PassPhase ->
        let g =
          g |> updateDurationAll
        in
          // ターン数更新
          match g |> turn with
          | 20 -> (g, Draw)
          | t  -> { g with Turn = t + 1 } |> procPhase SummonPhase

  let run plLftSpec plRgtSpec =
    let g = create plLftSpec plRgtSpec
    in g |> procPhase SummonPhase |> snd
