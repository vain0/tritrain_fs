namespace TriTrain.Core

open System
open Reflection

module Id =
  let create: unit -> Id =
    let r = ref 0
    let f () =
      (! r)
      |> tap (fun i -> r := i + 1)
      |> Id
    in f

module PlayerId =
  let all =
    DU<PlayerId>.UnitCases

  let inverse: PlayerId -> PlayerId =
    function
    | PlLft -> PlRgt
    | PlRgt -> PlLft

module CardId =
  let id          (cardId: CardId) = cardId.Id
  let owner       (cardId: CardId) = cardId.Owner

  let create (plId: PlayerId) =
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
  let coeff (src: Elem) (tar: Elem): float =
    if   isStrongTo src tar then 1.5
    elif isStrongTo tar src then 0.5
    else 1.0

module Vertex =
  let all =
    DU<Vertex>.UnitCases

module Row =
  let all =
    DU<Row>.UnitCases

  let ofVertex: Vertex -> Row =
    function
    | Fwd -> FwdRow
    | Lft
    | Rgt -> BwdRow

module ScopeSide =
  let all =
    DU<ScopeSide>.UnitCases

  /// plId からみた side 側のリスト
  let sides (plId: PlayerId) (side: ScopeSide): list<PlayerId> =
    match side with
    | Home -> [plId]
    | Oppo -> [plId |> PlayerId.inverse]
    | Both -> PlayerId.all

module ScopeForm =
  let name        ((name, _): ScopeForm) = name
  let typ         ((_, typ): ScopeForm) = typ

  let rec placeSetImpl ((plId, vx) as source) formType =
    match formType with
    | AbsForm (homeSet, oppoSet) ->
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

    | UnionForm forms ->
        forms
        |> List.map (placeSetImpl source)
        |> Set.unionMany

  let placeSet source (form: ScopeForm): Set<Place> =
    placeSetImpl source (form |> typ)

  let maxSize (form: ScopeForm): int =
    let rec body =
      function
      | AbsForm (home, oppo) ->
          Set.count home + Set.count oppo
      | FwdSide _ -> 1
      | BwdSide _ -> 2
      | LftSide _ -> 2
      | RgtSide _ -> 2
      | Self _ -> 1
      | FrontEnemy -> 1
      | UnionForm forms ->
          forms |> List.map body |> List.sum
    in form |> typ |> body

module Scope =
  let form        (scope: Scope) = scope.Form
  let aggregate   (scope: Scope) = scope.Aggregate

  let each form =
    {
      Form        = form
      Aggregate   = Each
    }

  let maxBy var form =
    {
      Form        = form
      Aggregate   = MaxBy (var, false)
    }

  let minBy var form =
    {
      Form        = form
      Aggregate   = MaxBy (var, true)
    }

module KEffect =
  let typ         (keff: KEffect) = keff.Type
  let duration    (keff: KEffect) = keff.Duration

  let create (typ: KEffectType) (duration: int): KEffect =
    {
      Type        = typ
      Duration    = duration
    }

module Skill =
  let rec toAtomList: Skill -> list<SkillAtom> =
    function
    | SkillAtom (name, oeffs) -> [(name, oeffs)]
    | SkillList skills -> skills |> List.collect toAtomList

  let toNameList (skill: Skill): list<Name> =
    skill |> toAtomList |> List.unzip |> fst

  let toEffectList (skill: Skill): list<OEffect> =
    skill |> toAtomList |> List.unzip |> snd |> List.collect id

  let name (skill: Skill): Name =
    match skill |> toNameList with
    | []      -> "Do nothing"
    | [name]  -> name
    | names   -> String.Join(" & ", names)

  let ofList: list<Skill> -> Skill =
    function
    | [skill] -> skill
    | skills -> skills |> SkillList

module Ability =
  let name        ((name, _): Ability) = name
  let condition   ((_, (cond, _)): Ability) = cond
  let effect      ((_, (_, oeff)): Ability) = oeff

  let add (abil: Ability) (abils: Map<TriggerCond, BatchedQueue<Ability>>) =
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

  let ofAtAg at ag: Status =
    {
      HP = StatusTotal - (at + ag)
      AT = at
      AG = ag
    }

  let toList (st: Status): list<int> =
    [ st |> hp; st |> at; st |> ag ]

  let total (st: Status): int =
    st |> toList |> List.sum

module CardSpec =
  let name        (spec: CardSpec) = spec.Name
  let status      (spec: CardSpec) = spec.Status
  let elem        (spec: CardSpec) = spec.Elem
  let abils       (spec: CardSpec) = spec.Abils
  let skills      (spec: CardSpec) = spec.Skills

  let abilList (cspec: CardSpec): list<Ability> =
    cspec |> abils
    |> Map.valueList
    |> List.collect (BatchedQueue.toList)

module Card =
  let cardId      (card: Card) = card.CardId
  let spec        (card: Card) = card.Spec
  let status      (card: Card) = card.Status
  let effects     (card: Card) = card.Effects

  let create (spec: CardSpec) (cardId: CardId): Card =
    {
      CardId        = cardId
      Spec          = spec
      Status        = spec |> CardSpec.status
      Effects       = []
    }

  let owner     = cardId >> CardId.owner
  let name      = spec >> CardSpec.name
  let elem      = spec >> CardSpec.elem
  let abils     = spec >> CardSpec.abils
  let maxHp     = spec >> CardSpec.status >> Status.hp

  let hp        = status >> Status.hp
  let at        = status >> Status.at
  let ag        = status >> Status.ag
  let isAlive   = hp >> flip (>) 0 
  let isDead    = isAlive >> not

  let isImmune card =
    card |> effects
    |> List.exists (function | { Type = (Immune | Haunted) } -> true | _ -> false)

  let isStable card =
    card |> effects
    |> List.exists (function | { Type = Stable } -> true | _ -> false)

  /// 地獄行きか？
  let isDamned card =
    card
    |> effects
    |> List.exists (function | { Type = (Damned | Haunted) } -> true | _ -> false)

  let setHp hp card =
    let hp = hp |> max 0 |> min (card |> maxHp) 
    in { card with Status = { (card |> status) with HP = hp } }
    
  let incAt amount card =
    let at' = card |> at |> (+) amount
    in { card with Status = { (card |> status) with AT = at' } }

  let incAg amount card =
    let ag' = card |> ag |> (+) amount
    in { card with Status = { (card |> status) with AG = ag' } }

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
      | HP ->
          match actor with
          | Some actor -> actor |> Card.hp |> float
          | None -> 0.0
      | AT ->
          match actor with
          | Some actor -> actor |> Card.at |> float
          | None -> 0.0
      | AG ->
          match actor with
          | Some actor -> actor |> Card.ag |> float
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
    | Immune
    | Stable
    | Damned
    | Haunted
      -> keff

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
  let create (cards: T7<CardSpec>) (plId: PlayerId): list<Card> =
    cards
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

  let emptyVertexSet (board: Board): Set<Vertex> =
    board
    |> Map.keySet
    |> Set.difference (Vertex.all |> Set.ofList)

module PlayerSpec =
  let name        (spec: PlayerSpec) = spec.Name
  let deck        (spec: PlayerSpec) = spec.Deck

  let create (name: Name) (deck: DeckSpec): PlayerSpec =
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

  let create (spec: PlayerSpec) (plId: PlayerId): (Player * list<Card>) =
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
