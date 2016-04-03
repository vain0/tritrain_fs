namespace TriTrain.Core

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
    [PlLft; PlRgt]

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

module Vertex =
  let all =
    [Fwd; Lft; Rgt]

module Row =
  let all =
    [FwdRow; BwdRow]

  let ofVertex =
    function
    | Fwd -> FwdRow
    | Lft
    | Rgt -> BwdRow

module ScopeSide =
  let all =
    [Home; Oppo; Both]

module Scope =
  /// plId からみた side 側のリスト
  let sides plId side =
    match side with
    | Home -> [plId]
    | Oppo -> [plId |> PlayerId.inverse]
    | Both -> PlayerId.all

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
            [ for pi in sides plId side -> (pi, Fwd) ]
            |> Set.ofList

    | BwdSide side ->
        match vx |> Row.ofVertex with
        | FwdRow ->
            [
              for pi in sides plId side do
                for p in [Lft; Rgt] -> (pi, p)
            ]
            |> Set.ofList
        | BwdRow -> Set.empty

    | LftSide side ->
        let sides = sides plId side
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
        let sides = sides plId side
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

module Status =
  let hp (st: Status) = st.HP
  let at (st: Status) = st.AT
  let ag (st: Status) = st.AG

module CardSpec =
  let name        (spec: CardSpec) = spec.Name
  let status      (spec: CardSpec) = spec.Status
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

module Amount =
  /// 変量を決定する
  let rec resolve (actor: option<Card>) (amount: Amount) =
    let rate = amount |> snd
    let value =
      match amount |> fst with
      | One -> rate
      | AT ->
          match actor with
          | Some actor -> actor |> Card.curAt |> float
          | None -> 0.0
    in value

module Deck =
  let create (spec) (plId) =
    spec
    |> T7.toList
    |> List.map (fun cardSpec ->
        let cardId = CardId.create plId
        in Card.create cardSpec cardId
        )

module Board =
  /// 空き頂点をつぶして、カードを移動させる。
  /// 移動後の盤面と、移動したカードのリストが返る。
  let rotate (board: Board): Board * list<CardId * Vertex * Vertex> =
    let (board', log) =
      board
      |> Map.toList
      |> List.sortBy fst    // 位置順
      |> List.zipShrink Vertex.all
      |> List.map
          (fun (v', (v, cardId)) ->
              let vc = (v', cardId)
              let log =  // カードの移動の記録
                if v = v'
                then None
                else Some (cardId, v, v')
              in (vc, log)
              )
      |> List.unzip
    let board'  = board' |> Map.ofList
    let log     = log |> List.choose id
    in (board', log)

  let emptyVertexSet board =
    board
    |> Map.keySet
    |> Set.difference (Vertex.all |> Set.ofList)

module PlayerSpec =
  let name        (spec: PlayerSpec) = spec.Name
  let deck        (spec: PlayerSpec) = spec.Deck

module Player =
  let playerId    (pl: Player) = pl.PlayerId
  let spec        (pl: Player) = pl.Spec
  let deck        (pl: Player) = pl.Deck
  let board       (pl: Player) = pl.Board
  let trash       (pl: Player) = pl.Trash

  let create spec plId =
    let deck' =
      Deck.create (spec |> PlayerSpec.deck) plId
    let pl =
      {
        PlayerId      = plId
        Spec          = spec
        Deck          = deck' |> List.map (Card.cardId)
        Board         = Map.empty
        Trash         = Set.empty
      }
    in (pl, deck')
