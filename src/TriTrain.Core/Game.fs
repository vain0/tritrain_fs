namespace TriTrain.Core

open Chessie.ErrorHandling

module Game =
  let plLft       (g: Game) = g.PlLft
  let plRgt       (g: Game) = g.PlRgt
  let cardMap     (g: Game) = g.CardMap
  let turn        (g: Game) = g.Turn
  let events      (g: Game) = g.Events
  let triggered   (g: Game) = g.Triggered

  let create (plLftSpec: PlayerSpec) (plRgtSpec: PlayerSpec): Game =
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

  let asObservable (g: Game): GameEventStream =
    (g |> events).AsObservable

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

  let modifyCard f cardId g =
    g |> updateCard (g |> card cardId |> f)

  let happen ev g =
    g |> tap (fun g -> (g |> events).Next(ev, g))

  let endIn r g =
    let g     = g |> happen (GameEnd r)
    let ()    = (g |> events).Completed()
    in (g, r)

  let trigger trig g =
    { g with Triggered = trig :: (g |> triggered) }
    |> happen (CardAbilityTrigger trig)

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

  let triggerAbils cond cardId g =
    match
      ( g |> searchBoardFor cardId
      , g |> card cardId |> Card.abils |> Map.tryFind cond
      ) with
    | (Some place, Some abils) ->
        g |> fold'
            (abils |> BatchedQueue.toList)
            (fun abil -> trigger (cardId, place, abil))
    | _ -> g

  /// カードを盤面に出す
  let summon cardId (plId, vx) g =
    let board = g |> board plId
    let () =
      assert (board |> Map.containsKey vx |> not)
      assert (g |> card cardId |> Card.isAlive)
    in
      g
      |> updateBoard plId
          (board |> Map.add vx cardId)
      |> happen (CardEnter (cardId, (plId, vx)))
      |> triggerAbils WhenEtB cardId

  let dieCard cardId g =
    match g |> searchBoardFor cardId with
    | None -> g
    | Some (plId, vx) ->
        // Die能力が誘発
        let g         = g |> triggerAbils WhenDie cardId

        // 盤面から除去
        let board'    = g |> board plId |> Map.remove vx
        let g         = g |> updateBoard plId board'
        // 再生効果
        let card'     = g |> card cardId |> Card.regenerate
        let g         = g |> updateCard card'

        if card' |> Card.isAlive then
          // 復活してボトムへ行く
          g
          |> updateDeck plId
              (g |> deck plId |> flip List.append [cardId])
          |> happen (CardRegenerate (cardId, card' |> Card.hp))
        elif card' |> Card.isDamned then
          // 復活せず、追放される
          g |> happen (CardIsExiled cardId)
        else // 復活せず、墓地へ行く
          g
          |> happen (CardDie cardId)
          |> updateTrash plId
              (g |> trash plId |> Set.add cardId)

  let incCardHp targetId amount g =
    let target    = g |> card targetId
    let hp'       = target |> Card.hp |> (+) amount
    let g         = g |> updateCard (target |> Card.setHp hp')
    let g         = g |> happen (CardHpInc (targetId, amount))
    let g =
      if g |> card targetId |> Card.isDead
      then g |> dieCard targetId
      else g
    in g

  /// 継続的効果を取得するときの処理
  let onGainKEffect targetId keff g =
    match keff |> KEffect.typ with
    | ATInc (One, value) ->
        g |> modifyCard (Card.incAt (value |> int)) targetId
    | AGInc (One, value) ->
        g |> modifyCard (Card.incAg (value |> int)) targetId
    | ATInc _
    | AGInc _ -> failwith "never"
    | Curse _
    | Regenerate _
    | Immune
    | Stable
    | Damned
    | Haunted
      -> g

  /// 継続的効果が消失するときの処理
  let onLoseKEffect targetId keff g =
    match keff |> KEffect.typ with
    | ATInc (One, value) ->
        g |> modifyCard (Card.incAt (value |> int |> (~-))) targetId
    | AGInc (One, value) ->
        g |> modifyCard (Card.incAg (value |> int |> (~-))) targetId
    | ATInc _
    | AGInc _ -> failwith "never"
    | Haunted ->
        g |> dieCard targetId
    | Curse _
    | Regenerate _
    | Immune
    | Stable
    | Damned
      -> g

  let giveKEffect targetId keff g =
    let target    = g |> card targetId
    if target |> Card.isStable then
      g |> happen (CardNullifyEffect (targetId, Give keff))
    else
      let effs'     = keff :: (target |> Card.effects)
      let g         = g |> updateCard { target with Effects = effs' }
      let g         = g |> happen (CardGainEffect (targetId, keff))
      let g         = g |> onGainKEffect targetId keff
      in g

  /// カードから、selector が真を返すような継続的効果を失わせる。
  /// (恒常は作用しない。)
  let loseKEffect targetId selector g =
    let target          = g |> card targetId
    let (card', lost)   = target |> Card.loseEffects selector
    in
      g
      |> updateCard card'
      |> fold' lost (fun keff g ->
          g
          |> happen (CardLoseEffect (targetId, keff))
          |> onLoseKEffect targetId keff
          )

  let cancelKEffect keffcan targetId g =
    if g |> card targetId |> Card.isStable then
      g |> happen (CardNullifyEffect (targetId, Cancel keffcan))
    else
      g |> loseKEffect targetId (KEffect.isCancelledBy keffcan)

  let rec procOEffectToUnit oeffType actorIdOpt targetId g =
    let target    = g |> card targetId
    let actorOpt  = actorIdOpt |> Option.map (fun actorId -> g |> card actorId)
    let oeffType  = Amount.resolveOEffectToUnit actorOpt target oeffType
    let g =
      match oeffType with
      | Damage amount ->
          if target |> Card.isImmune then
            g |> happen (CardNullifyEffect (targetId, oeffType))
          else
            let coeffByElem =
              match actorOpt with
              | Some actor ->
                  Elem.coeff (actor |> Card.elem) (target |> Card.elem)
              | None -> 1.0
            let amount = amount |> snd |> (*) coeffByElem |> int |> max 0
            in g |> incCardHp targetId (- amount)
      | Heal amount ->
          g |> incCardHp targetId (amount |> snd |> int |> max 0)
      | Hex amount ->
          let prob   = amount |> snd |> flip (/) 100.0
          let g =
            if Random.roll prob then
              let target  = g |> card targetId
              if target |> Card.isHaunted then
                g |> happen (CardNullifyEffect (targetId, oeffType))
              else
                let amount  = target |> Card.hp |> (~-)
                let g       = g |> incCardHp targetId amount
                in g
            else g
          in g
      | Give keff ->
          g |> giveKEffect targetId keff
      | Cancel keffcan ->
          g |> cancelKEffect keffcan targetId
    in g

  let findTargets source scope g =
    let places =
      scope |> Scope.form
      |> ScopeForm.placeSet source
      |> Set.toList
    let targets () =  // 生存しているカードの列
      places
      |> List.choose (fun (plId, vx) ->
          g |> board plId |> Map.tryFind vx
          )
    in
      match scope |> Scope.aggregate with
      | Each -> targets ()
      | MaxBy (var, rev) ->
          targets ()
          |> List.tryMaxBy (fun cardId ->
              (var, if rev then -1.0 else 1.0)
              |> Amount.resolve (g |> card cardId |> Some)
              )
          |> Option.toList

  let resurrect actorIdOpt amount g =
    trial {
      let! actorId  = actorIdOpt |> failIfNone ()
      let  actor    = g |> card actorId
      let  plId     = actor |> Card.owner
      let  trash    = g |> trash plId
      let! tarId    = trash |> Random.element |> failIfNone ()
      let  board    = g |> board plId
      let! vx       = board |> Board.emptyVertexSet |> Random.element |> failIfNone ()
      let  trash'   = trash |> Set.filter ((<>) tarId)
      let  tar      = g |> card tarId
      let  rate     = Amount.resolve (Some actor) amount |> flip (/) 100.0
      let  hp       = tar |> Card.maxHp |> float |> (*) rate |> int
      let  tar'     = tar |> Card.setHp hp
      if hp <= 0 then return! fail ()
      return
        g
        |> updateCard tar'
        |> updateTrash plId trash'
        |> summon tarId (plId, vx)
    } |> Trial.either fst (fun _ -> g)

  /// moves: (移動するカードのID, 元の位置, 後の位置) の列
  /// 移動後の盤面の整合性は、利用側が担保すること。
  let moveCards (moves: list<CardId * Place * Place>) g =
    g
    |> fold' moves (fun (cardId, (plId, vx), _) g ->
        g |> updateBoard plId  (g |> board plId  |> Map.remove vx)
        )
    |> fold' moves (fun (cardId, _, (plId', vx')) g ->
        g |> updateBoard plId' (g |> board plId' |> Map.add vx' cardId)
        )
    |> happen (CardMove moves)

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

  /// 盤面を回転させる
  let rotateBoard plId g =
    let moves =
      g
      |> board plId
      |> Board.rotate
      |> List.map (fun (cardId, vx, vx') ->
          (cardId, (plId, vx), (plId, vx'))
          )
    in g |> moveCards moves

  /// 行動者が静的条件を満たしているか？
  let satisfy (cond: StaticCond) actorIdOpt g =
    match cond with
    | Resonance elem ->
        match actorIdOpt with
        | None -> false
        | Some actorId ->
            g |> board (actorId |> CardId.owner)
            |> Seq.map (fun (KeyValue (_, cardId)) ->
                g |> card cardId |> Card.elem
                )
            |> Seq.toList  // for structural equality
            |> (=) (List.replicate 3 elem)

  let rec procOEffect oeff (actorIdOpt: option<CardId>) (source: Place) g =
    match oeff with
    | GenToken cardSpecs ->
        g // TODO: トークン生成

    | Resurrect amount ->
        g |> resurrect actorIdOpt amount

    | Swap form ->
        match form |> ScopeForm.placeSet source |> Set.toList with
        | [r1; r2] -> g |> swapCards r1 r2
        | _ -> g

    | Rotate scopeSide ->
        g |> fold'
            (ScopeSide.sides (source |> fst) scopeSide)
            rotateBoard

    | OEffectToUnits (typ, scope) ->
        g |> fold'
            (g |> findTargets source scope)
            (procOEffectToUnit typ actorIdOpt)

    | AsLongAs (cond, then', else') ->
        g |> procOEffectList actorIdOpt source
            (if g |> satisfy cond actorIdOpt then [then'] else else' |> Option.toList)

  and procOEffectList actorIdOpt source oeffs g =
    let loop oeff g =
      let source =  // actor の最新の位置に更新する
        actorIdOpt
        |> Option.bind
            (fun actorId -> g |> searchBoardFor actorId)
        |> Option.getOr source
      in g |> procOEffect oeff actorIdOpt source
    in
      g |> fold' oeffs loop

  /// 未行動な最速カード
  let tryFindFastest actedCards g: option<Vertex * CardId> =
    g
    |> placeMap
    |> Map.toList  // 位置順
    |> List.choose (fun ((_, vx), cardId) ->
        if actedCards |> Set.contains cardId
        then None
        else (g |> card cardId |> Card.ag, (vx, cardId)) |> Some
        )
    |> List.tryMaxBy fst
    |> Option.map snd

  /// カード actor の行動を処理する
  let procAction actorId vx g =
    match g |> card actorId |> Card.tryGetActionOn vx with
    | None -> g
    | Some skill ->
        g
        |> happen (CardBeginAction (actorId, skill))
        |> procOEffectList
            (Some actorId) (actorId |> CardId.owner, vx)
            (skill |> Skill.toEffectList)

  /// 誘発した能力を解決する
  let rec solveTriggered g =
    match g |> triggered with
    | [] -> g
    | trig :: triggered' ->
        let (actorId, source, (_, (_, oeffs))) = trig
        let source    = g |> searchBoardFor actorId |> Option.getOr source
        in
          { g with Triggered = triggered' }
          |> happen (SolveTriggered trig)
          |> procOEffectList (Some actorId) source oeffs
          |> solveTriggered

  /// プレイヤー plId が位置 vx にデッキトップを召喚する。
  /// デッキが空なら何もしない。
  let summonFromTop (plId, vx) g =
    match g |> deck plId with
    | [] -> g
    | cardId :: deck' ->
        g
        |> updateDeck plId deck'
        |> summon cardId (plId, vx)

  let procSummonPhase plId g =
    g |> fold'
        (g |> board plId |> Board.emptyVertexSet)
        (fun vx -> summonFromTop (plId, vx))

  /// 全体に再生効果をかける
  let procRegenerationPhase g =
    let body plId g =
      let keff =
        let rate =
          match plId with
          | PlLft -> 50.0
          | PlRgt -> 60.0
        in
          KEffect.create (Regenerate (One, rate)) 10
      let targets =
        g |> cardMap
        |> Map.filter (fun _ card -> card |> Card.owner = plId)
      in
        g |> fold' targets
            (fun (KeyValue (cardId, _)) -> procOEffectToUnit (Give keff) None cardId)
    in
      g |> fold' (PlayerId.all) body

  let triggerBoTAbils g =
    g |> fold' (g |> placeMap)
        (fun (KeyValue (_, cardId)) -> triggerAbils WhenBoT cardId)

  /// 奇数ターンなら、後攻側の各カードが自己加速する
  let procWindPhase cond g =
    if cond then
      let keff      = KEffect.create (AGInc (AG, 0.10)) 1
      let oeff      = OEffectToUnits (Give keff, Preset.Scope.self)
      let targets   =
        g |> placeMap
        |> Map.filter (fun _ cardId -> cardId |> CardId.owner = PlRgt)
      in
        g |> fold' targets
            (fun (KeyValue (source, actorId)) -> procOEffect oeff (actorId |> Some) source)
    else g

  /// 呪いの効果を処理する
  let procCurse g =
    g |> fold' (g |> placeMap)
        (fun (KeyValue (place, cardId)) g ->
            match g |> card cardId |> Card.curseTotal with
            | 0 -> g
            | total ->
                g
                |> happen (CardIsCursed (cardId, total))
                |> procOEffectToUnit (Damage (One, float total)) None cardId
                |> cancelKEffect CurseCanceller cardId
            )

  /// カードにかかっている継続的効果の経過ターン数を更新する
  let updateDuration cardId g =
    g
    |> updateCard (g |> card cardId |> Card.decDuration)
    |> loseKEffect cardId (function | { Duration = 0 } -> true | _ -> false)
  
  let updateDurationAll g =
    g |> fold'
        (g |> cardIdsOnBoard)
        updateDuration

  let rec procPhaseImpl ph g =
    match ph with
    | SummonPhase ->
        let g =
          g
          |> happen TurnBegin
          |> fold' (PlayerId.all) procSummonPhase
        let isLost plId =
          g |> board plId |> Map.isEmpty
        in
          // 勝敗判定
          match PlayerId.all |> List.filter isLost with
          | []      -> g |> procPhase UpkeepPhase
          | [plId]  -> g |> endIn (plId |> PlayerId.inverse |> Win)
          | _       -> g |> endIn Draw

    | UpkeepPhase ->
        g
        |> triggerBoTAbils
        |> procPhase (WindPhase (g |> turn |> flip (%) 2 |> (=) 1))

    | WindPhase blows ->
        g
        |> procWindPhase blows
        |> procPhase (ActionPhase Set.empty)

    | ActionPhase actedCards ->
        match g |> tryFindFastest actedCards with
        | Some (vx, actorId) ->
            g
            |> procAction actorId vx
            |> procPhase (actedCards |> Set.add actorId |> ActionPhase)
        | None ->
            g |> procPhase RotatePhase

    | RotatePhase ->
        g
        |> fold' (PlayerId.all) rotateBoard
        |> procPhase PassPhase

    | PassPhase ->
        let g =
          g |> procCurse |> updateDurationAll
        in
          // ターン数更新
          match g |> turn with
          | MaxTurns -> g |> endIn Draw
          | t  -> { g with Turn = t + 1 } |> procPhase SummonPhase

  /// 誘発した能力を解決してからフェイズ処理をする
  and procPhase ph g =
    g |> solveTriggered |> procPhaseImpl ph

  let run g: Game * GameResult =
    g
    |> happen GameBegin
    |> procRegenerationPhase
    |> procPhase SummonPhase
