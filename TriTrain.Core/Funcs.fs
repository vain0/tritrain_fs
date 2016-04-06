namespace TriTrain.Core

open System
open Reflection

module Id =
  let create =
    let r = ref 0
    let f () =
      (! r)
      |> tap (fun i -> r := i + 1)
      |> Id
    in f

module PlayerId =
  let all =
    DU<PlayerId>.UnitCases

  let inverse =
    function
    | PlLft -> PlRgt
    | PlRgt -> PlLft

module CardId =
  let id          (cardId: CardId) = cardId.Id
  let owner       (cardId: CardId) = cardId.Owner

  let create plId =
    {
      Owner       = plId
      Id          = Id.create ()
    }

module Elem =
  let all =
    DU<Elem>.UnitCases

  let isStrongTo src tar =
    match (src, tar) with
    | (Air  , Earth)
    | (Earth, Water)
    | (Water, Fire )
    | (Fire , Air  ) -> true
    | _ -> false

  /// 属性 src が属性 tar を攻撃するときにかかる係数
  let coeff src tar =
    if   isStrongTo src tar then 1.5
    elif isStrongTo tar src then 0.5
    else 1.0

module Vertex =
  let all =
    DU<Vertex>.UnitCases

module Row =
  let all =
    DU<Row>.UnitCases

  let ofVertex =
    function
    | Fwd -> FwdRow
    | Lft
    | Rgt -> BwdRow

module ScopeSide =
  let all =
    DU<ScopeSide>.UnitCases

  /// plId からみた side 側のリスト
  let sides plId side =
    match side with
    | Home -> [plId]
    | Oppo -> [plId |> PlayerId.inverse]
    | Both -> PlayerId.all

module Scope =
  let name ((name, _): NamedScope) = name

  let rec placeSet ((plId, vx) as source) scope: Set<Place> =
    match scope with
    | AbsScope (homeSet, oppoSet) ->
        [
          for p in homeSet -> (plId, p)
          for p in oppoSet -> (plId |> PlayerId.inverse, p)
        ]
        |> Set.ofList

    | FwdSide side ->
        match vx |> Row.ofVertex with
        | FwdRow -> Set.empty
        | BwdRow ->
            [ for pi in ScopeSide.sides plId side -> (pi, Fwd) ]
            |> Set.ofList

    | BwdSide side ->
        match vx |> Row.ofVertex with
        | FwdRow ->
            [
              for pi in ScopeSide.sides plId side do
                for p in [Lft; Rgt] -> (pi, p)
            ]
            |> Set.ofList
        | BwdRow -> Set.empty

    | LftSide side ->
        let sides = ScopeSide.sides plId side
        let fwd =
          match vx with
          | Lft -> []
          | Fwd | Rgt -> [ for side in sides -> (side, Fwd) ]
        let lft =
          match vx with
          | Lft | Fwd -> []
          | Rgt -> [ for side in sides -> (side, Lft) ]
        in (List.append fwd lft) |> Set.ofList

    | RgtSide side ->
        let sides = ScopeSide.sides plId side
        let fwd =
          match vx with
          | Lft | Fwd -> [ for side in sides -> (side, Fwd) ]
          | Rgt -> []
        let rgt =
          match vx with
          | Lft -> [ for side in sides -> (side, Rgt) ]
          | Fwd | Rgt -> []
        in  (List.append fwd rgt) |> Set.ofList

    | Self ->
        Set.singleton source

    | FrontEnemy ->
        Set.singleton ((PlayerId.inverse plId), vx)

    | UnionScope scopes ->
        scopes
        |> List.map (placeSet source)
        |> Set.unionMany

module KEffect =
  let typ         (keff: KEffect) = keff.Type
  let duration    (keff: KEffect) = keff.Duration

  let create typ duration =
    {
      Type        = typ
      Duration    = duration
    }

module Skill =
  let rec toAtomList =
    function
    | SkillAtom (name, oeffs) -> [(name, oeffs)]
    | SkillList skills -> skills |> List.collect toAtomList

  let toNameList skill =
    skill |> toAtomList |> List.unzip |> fst

  let toEffectList skill =
    skill |> toAtomList |> List.unzip |> snd |> List.collect id

  let name skill =
    match skill |> toNameList with
    | []      -> "Do nothing"
    | [name]  -> name
    | names   -> String.Join(" & ", names)

  let ofList =
    function
    | [skill] -> skill
    | skills -> skills |> SkillList

module Ability =
  let name        ((name, _): Ability) = name
  let condition   ((_, (cond, _)): Ability) = cond
  let effect      ((_, (_, oeff)): Ability) = oeff

  let add abil abils =
    let cond = abil |> condition
    let q =
      match abils |> Map.tryFind cond with
      | None    -> BatchedQueue.singleton abil
      | Some q  -> q |> BatchedQueue.add abil
    in abils |> Map.add cond q

module Status =
  let hp (st: Status) = st.HP
  let at (st: Status) = st.AT
  let ag (st: Status) = st.AG

  let ofAtAg at ag =
    {
      HP = StatusTotal - (at + ag)
      AT = at
      AG = ag
    }

  let toList st =
    [ st |> hp; st |> at; st |> ag ]

  let total st =
    st |> toList |> List.sum

module CardSpec =
  let name        (spec: CardSpec) = spec.Name
  let status      (spec: CardSpec) = spec.Status
  let elem        (spec: CardSpec) = spec.Elem
  let abils       (spec: CardSpec) = spec.Abils
  let skills      (spec: CardSpec) = spec.Skills

module Card =
  let cardId      (card: Card) = card.CardId
  let spec        (card: Card) = card.Spec
  let curHp       (card: Card) = card.CurHP
  let effects     (card: Card) = card.Effects

  let create spec cardId =
    {
      CardId        = cardId
      Spec          = spec
      CurHP         = spec |> CardSpec.status |> Status.hp
      Effects       = []
    }

  let owner =
    cardId >> CardId.owner

  let name =
    spec >> CardSpec.name

  let elem =
    spec >> CardSpec.elem

  let abils =
    spec >> CardSpec.abils

  let maxHp =
    spec >> CardSpec.status >> Status.hp

  let isAlive card =
    curHp card > 0

  let isDead =
    isAlive >> not

  let curAt card =
    card
    |> effects
    |> List.map (fun keff ->
        match keff |> KEffect.typ with
        | ATInc (One, value) -> value
        | _ -> 0.0
        )
    |> List.sum
    |> (+) (card |> spec |> CardSpec.status |> Status.at |> float)
    |> int

  let curAg card =
    card
    |> effects
    |> List.map (fun keff ->
        match keff |> KEffect.typ with
        | AGInc (One, value) -> value
        | _ -> 0.0
        )
    |> List.sum
    |> (+) (card |> spec |> CardSpec.status |> Status.ag |> float)
    |> int

  let curStatus card =
    {
      HP = card |> curHp 
      AT = card |> curAt
      AG = card |> curAg
    }

  let setHp hp card =
    let hp = hp |> max 0 |> min (card |> maxHp) 
    in { card with CurHP = hp }

  /// 再生効果を適用する
  let regenerate card =
    let (regenValues, effects') =
      card
      |> effects
      |> List.paritionMap
          (function
          | { Type = (Regenerate (One, value)) } -> Some value
          | _ -> None
          )
    let rate = regenValues |> List.sum |> flip (/) 100.0
    let card =
      { card with Effects = effects' }
      |> setHp (card |> maxHp |> float |> (*) rate |> int)
    in card

  /// カード actor が位置 vx で起こす行動を取得する。
  let tryGetActionOn vx actor: option<Skill> =
    actor
    |> spec
    |> CardSpec.skills
    |> Map.tryFind (vx |> Row.ofVertex)

module Amount =
  /// 変量を決定する
  let rec resolve (actor: option<Card>) (amount: Amount) =
    let rate = amount |> snd
    let value =
      match amount |> fst with
      | One -> 1.0
      | MaxHP ->
          match actor with
          | Some actor -> actor |> Card.maxHp |> float
          | None -> 0.0
      | AT ->
          match actor with
          | Some actor -> actor |> Card.curAt |> float
          | None -> 0.0
      | AG ->
          match actor with
          | Some actor -> actor |> Card.curAg |> float
          | None -> 0.0
    in value * rate

  /// 継続的効果の変量を固定する。
  /// 事後: 含まれる Amount はすべて (One, _) である。
  let resolveKEffect actorOpt target keff =
    match keff |> KEffect.typ with
    | ATInc amount ->
        { keff with Type = ATInc (One, amount |> resolve actorOpt) }
    | AGInc amount ->
        { keff with Type = AGInc (One, amount |> resolve actorOpt) }
    | Regenerate amount ->
        { keff with Type = Regenerate (One, amount |> resolve actorOpt) }

  /// 単発的効果の変量を固定する。
  /// 事後: 含まれる Amount はすべて (One, _) である。
  let resolveOEffectToUnit actorOpt target oeffType =
    match oeffType with
    | Damage amount ->
        let amount = amount |> resolve actorOpt
        in Damage (One, amount)
    | Heal amount ->
        let amount = amount |> resolve actorOpt
        in Heal (One, amount)
    | Death  amount ->
        let amount = amount |> resolve actorOpt
        in Death (One, amount)
    | Give keff ->
        keff |> resolveKEffect actorOpt target |> Give

module DeckSpec =
  let name        (spec: DeckSpec) = spec.Name
  let cards       (spec: DeckSpec) = spec.Cards

module Deck =
  let create (spec) (plId) =
    spec
    |> T7.toList
    |> List.map (fun cardSpec ->
        let cardId = CardId.create plId
        in Card.create cardSpec cardId
        )

module Board =
  /// 空き頂点をつぶしてカードを移動させるために必要な、
  /// 具体的なカードの移動を計算する。
  let rotate (board: Board): list<CardId * Vertex * Vertex> =
    board
    |> Map.toList
    |> List.sortBy fst    // 位置順
    |> List.zipShrink Vertex.all
    |> List.choose
        (fun (v', (v, cardId)) ->
            if v = v'
            then None
            else Some (cardId, v, v')
            )

  let emptyVertexSet board =
    board
    |> Map.keySet
    |> Set.difference (Vertex.all |> Set.ofList)

module PlayerSpec =
  let name        (spec: PlayerSpec) = spec.Name
  let deck        (spec: PlayerSpec) = spec.Deck

  let create name deck =
    {
      Name          = name
      Deck          = deck
    }

module Player =
  let playerId    (pl: Player) = pl.PlayerId
  let spec        (pl: Player) = pl.Spec
  let deck        (pl: Player) = pl.Deck
  let board       (pl: Player) = pl.Board
  let trash       (pl: Player) = pl.Trash

  let create spec plId =
    let deck' =
      Deck.create (spec |> PlayerSpec.deck |> DeckSpec.cards) plId
    let pl =
      {
        PlayerId      = plId
        Spec          = spec
        Deck          = deck' |> List.map (Card.cardId)
        Board         = Map.empty
        Trash         = Set.empty
      }
    in (pl, deck')

  let name =
    spec >> PlayerSpec.name

module GameResult =
  let all =
    [Win PlLft; Win PlRgt; Draw]
