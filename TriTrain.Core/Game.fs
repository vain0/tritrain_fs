namespace TriTrain.Core

module Game =
  let plLft       (g: Game) = g.PlLft
  let plRgt       (g: Game) = g.PlRgt
  let cardMap     (g: Game) = g.CardMap
  let turn        (g: Game) = g.Turn
  let events      (g: Game) = g.Events

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
        Events      = Observable.Source<GameEvent * Game>()
      }

  let asObservable g =
    (g |> events).AsObservable
    |> Observable.duplicateFirst
    |> Observable.pairwise
    |> Observable.map (fun ((_, g), (ev, g')) -> (ev, g, g'))

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

  let happen ev g =
    g |> tap (fun g -> (g |> events).Next(ev, g))

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
          |> happen (CardDie cardId)

  let incCardHp targetId amount g =
    let target    = g |> card targetId
    let hp'       = target |> Card.curHp |> (+) amount |> max 0 
    let g         = g |> updateCard { target with CurHP = hp' }
    let g         = g |> happen (CardHpInc (targetId, amount))
    let g =
      if hp' = 0
      then g |> dieCard targetId
      else g
    in g

  let giveKEffectTo targetId keff g =
    let target    = g |> card targetId
    let effs'     = keff :: (target |> Card.effects)
    let g         = g |> updateCard { target with Effects = effs' }
    let g         = g |> happen (CardGainEffect (targetId, keff))
    in g

  let rec procOEffectToUnit actorOpt targetId oeffType g =
    match oeffType with
    | Damage amount ->
        let target = g |> card targetId
        let coeffByElem =
          match actorOpt with
          | Some actor ->
              Elem.coeff (actor |> Card.elem) (target |> Card.elem)
          | None -> 1.0
        let amount =
          amount
          |> Amount.resolve actorOpt
          |> (*) coeffByElem
          |> int
          |> max 0
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

    | Give keff ->
        g |> giveKEffectTo targetId keff

  /// moves: (移動するカードのID, 元の位置, 後の位置) の列
  /// 移動後の盤面の整合性は、利用側が担保すること。
  let moveCards (moves: list<CardId * Place * Place>) g =
    let g =
      moves |> List.fold (fun g (cardId, (plId, vx), _) ->
          g |> updateBoard plId  (g |> board plId  |> Map.remove vx)
          ) g
    let g =
      moves |> List.fold (fun g (cardId, _, (plId', vx')) ->
          g |> updateBoard plId' (g |> board plId' |> Map.add vx' cardId)
          ) g
    let g =
      // 移動を通知
      moves |> List.fold (fun g l ->
          g |> happen (CardMove l)
          ) g
    in g

  let swapCards r1 r2 g =
    let placeMap = g |> placeMap
    let opt1 = placeMap |> Map.tryFind r1
    let opt2 = placeMap |> Map.tryFind r2
    let g =
      match (opt1, opt2) with
      | (Some cardId1, Some cardId2)->
          g |> moveCards
            [ (cardId1, r1, r2)
              (cardId2, r2, r1) ]
      | _ -> g
    in g

  let rec procOEffect actorOpt (source: Place) oeff g =
    match oeff with
    | OEffectList oeffs ->
        oeffs |> List.fold (flip (procOEffect actorOpt source)) g
    | GenToken cardSpecs ->
        g // TODO: トークン生成

    | Swap (_, scope) ->
        match scope |> Scope.placeSet source |> Set.toList with
        | [r1; r2] -> g |> swapCards r1 r2
        | _ -> g

    | OEffectToUnits (typ, (_, scope)) ->
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
      | Some ((_, oeff) as noeff) ->
          g
          |> happen (CardActBegin (actorId, noeff))
          |> procOEffect (Some actor) (actorId |> CardId.owner, vx) oeff
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
          |> happen (CardEnter (cardId, (plId, vx)))
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
    let (effects', endEffects') =
      card
      |> Card.effects
      |> List.map (fun keff ->
          match keff.Duration with
          | None -> (Some keff, None)
          | Some n ->
              if n <= 1
              then (None, Some keff)
              else ({ keff with Duration = Some (n - 1) } |> Some, None)
          )
      |> List.unzip
    let card' = { card with Effects = effects' |> List.choose id }
    let g =
      endEffects'
      |> List.choose id
      |> List.fold (fun g keff ->
          g |> happen (CardLoseEffect (cardId, keff))
          ) g
    in g |> updateCard card'
  
  let updateDurationAll g =
    g
    |> cardIdsOnBoard
    |> Set.fold (fun g cardId ->
        g |> updateDuration cardId
        ) g

  let endWith r g =
    g |> happen (GameEnd r)

  let rec procPhase ph g: Game =
    match ph with
    | SummonPhase ->
        let g =
          g |> happen TurnBegin
        let g =
          PlayerId.all
          |> List.fold (fun g plId -> g |> procSummonPhase plId) g
        let isLost plId =
          g |> board plId |> Map.isEmpty
        in
          // 勝敗判定
          match PlayerId.all |> List.filter isLost with
          | []      -> g |> procPhase UpkeepPhase
          | [plId]  -> g |> endWith (plId |> PlayerId.inverse |> Win)
          | _       -> g |> endWith Draw

    | UpkeepPhase ->
        // TODO: BoT能力が誘発
        g |> procPhase (ActionPhase Set.empty)

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
            let g = g |> updateBoard plId board'
            let g =
              // 移動を通知
              log |> List.fold (fun g (cardId, v, v') ->
                  g |> happen (CardMove (cardId, (plId, v), (plId, v')))
                  ) g
            in g
            ) g
        |> procPhase PassPhase

    | PassPhase ->
        let g =
          g |> updateDurationAll
        in
          // ターン数更新
          match g |> turn with
          | 20 -> g |> endWith Draw
          | t  -> { g with Turn = t + 1 } |> procPhase SummonPhase

  let run g: Game =
    g
    |> happen GameBegin
    |> procPhase SummonPhase
