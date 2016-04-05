namespace TriTrain.Core

[<AutoOpen>]
module Types =
  type Id =
    | Id of int

  type CardName = string

  type Vertex =
    | Fwd
    | Lft
    | Rgt

  type Row =
    /// forward row
    | FwdRow
    /// backward row
    | BwdRow

  /// 四元素(elements)
  type Elem =
    | Air
    | Fire
    | Water
    | Earth

  type PlayerId =
    | PlLft
    | PlRgt

  type Place =
    PlayerId * Vertex

  type ScopeSide =
    | Home
    | Oppo
    | Both

  /// 効果の影響範囲
  type Scope =
    | AbsScope      of home: Set<Vertex> * oppo: Set<Vertex>
    | FwdSide       of ScopeSide
    | BwdSide       of ScopeSide
    | LftSide       of ScopeSide
    | RgtSide       of ScopeSide
    | Self
    | FrontEnemy
    | UnionScope    of list<Scope>

  type ScopeName = string

  type NamedScope =
    ScopeName * Scope

  /// 変量
  type VarType =
    | One
    | MaxHP  // unused
    | AT
    | AG

  type Amount =
    VarType * Rate

  type Duration =
    option<int>

  /// クリーチャーに作用する継続的効果
  type KEffectType =
    | ATInc         of Amount
    | AGInc         of Amount
    | Regenerate    of Amount

  /// 継続的効果 (Continuous Effect)
  type KEffect =
    {
      Type          : KEffectType
      Duration      : Duration
    }

  /// 盤面上のカード単体に作用する単発的効果
  type OEffectToUnitType =
    | Damage        of Amount
    | Heal          of Amount
    | Death         of Amount
    | Give          of KEffect
    //| Unsummon

  /// 単発的効果 (Oneshot Effect)
  type OEffectAtom =
    | OEffectToUnits
      of OEffectToUnitType * NamedScope
    | Swap          of NamedScope
    | Rotate        of ScopeSide
    | GenToken      of list<CardSpec>

  and OEffect =
    | OEffectList   of list<OEffect>
    | OEffectAtom   of OEffectAtom

  and NamedOEffect =
    string * OEffect

  and TriggerCond =
    /// At the beginning of each turn
    | WhenBoT
    /// When entering the battlefield
    | WhenEtB
    | WhenDie
    //| WhenDealt

  and Ability =
    {
      Cond          : TriggerCond
      Effect        : NamedOEffect
    }

  and Status =
    {
      HP            : int
      AT            : int
      AG            : int
    }

  and CardSpec =
    {
      Name          : CardName
      Status        : Status
      Elem          : Elem
      Abils         : list<Ability>
      Skills        : Map<Row, NamedOEffect>
    }

  type CardId =
    {
      Owner         : PlayerId
      Id            : Id
    }

  type Card =
    {
      CardId        : CardId
      Spec          : CardSpec
      CurHP         : int
      Effects       : list<KEffect>
    }

  type DeckSpec =
    {
      Name          : string
      Cards         : T7<CardSpec>
    }

  type Deck =
    list<CardId>

  type Board =
    Map<Vertex, CardId>

  type Trash =
    Set<CardId>

  type PlayerSpec =
    {
      Name          : string
      Deck          : DeckSpec
    }

  type Player =
    {
      PlayerId      : PlayerId
      Spec          : PlayerSpec
      Deck          : Deck
      Board         : Board
      Trash         : Trash
    }

  type Phase =
    | SummonPhase
    | UpkeepPhase
    | ActionPhase   of actedCards: Set<CardId>
    | RotatePhase
    | PassPhase

  type GameResult =
    | Win           of PlayerId
    | Draw

  type GameEvent =
    | GameBegin
    | GameEnd             of GameResult
    | TurnBegin
    | WindBlow
    | CardEnter           of CardId * Place
    | CardBeginAction     of CardId * NamedOEffect
    | CardHpInc           of CardId * amount: int
    | CardRegenerate      of CardId * amount: int
    | CardDie             of CardId
    | CardGainEffect      of CardId * KEffect
    | CardLoseEffect      of CardId * KEffect
    | CardMove            of list<CardId * (* src: *) Place * (* dst: *) Place>

  type Game =
    {
      PlLft         : Player
      PlRgt         : Player
      CardMap       : Map<CardId, Card>
      Turn          : int

      /// 誘発し、まだ処理されていない誘発型能力
      Triggered     : list<OEffect>

      Events        : Observable.Source<GameEvent * Game>
    }

  type AbilitySrc =
    {
      Cond          : string
      Effect        : string
    }

  /// ユーザが記述するカード仕様
  type CardSpecSrc =
    {
      Name          : CardName
      AT            : int
      AG            : int
      Elem          : string
      Abils         : list<AbilitySrc>
      SkillFwd      : list<string>
      SkillBwd      : list<string>
    }

  type DeckSpecSrc =
    {
      Name          : string
      Cards         : T7<CardSpecSrc>
    }

  [<Literal>]
  let StatusTotal = 300

  [<Literal>]
  let MaxDefaultAG = 50
