namespace TriTrain.Core

open System

[<AutoOpen>]
module Types =
  type Id =
    | Id of int

  type Name = string
  type CardName = Name
  type SkillName = Name
  type AbilName = Name

  type Vertex =
    | Fwd
    | Lft
    | Rgt

  /// 変量
  type VarType =
    | One
    | MaxHP  // unused
    | HP
    | AT
    | AG

  type Amount =
    VarType * Rate

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
  type ScopeFormType =
    | AbsForm       of home: Set<Vertex> * oppo: Set<Vertex>
    | FwdSide       of ScopeSide
    | BwdSide       of ScopeSide
    | LftSide       of ScopeSide
    | RgtSide       of ScopeSide
    | Self
    | FrontEnemy
    | UnionForm     of list<ScopeFormType>

  type ScopeForm =
    Name * ScopeFormType

  /// Aggregation
  type ScopeAggregate =
    | Each
    | MaxBy         of VarType * rev: bool

  type Scope =
    {
      Form          : ScopeForm
      Aggregate     : ScopeAggregate
    }

  type Duration =
    int

  type KEffectCanceller =
    | AgIncCanceller
    | CurseCanceller
    | ImmuneCanceller

  /// クリーチャーに作用する継続的効果
  type KEffectType =
    | ATInc         of Amount
    | AGInc         of Amount
    | Curse         of Amount
    | Regenerate    of Amount
    | Immune
    | Stable
    | Damned
    | Haunted

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
    | Hex           of Amount
    | Give          of KEffect
    | Cancel        of KEffectCanceller
    //| Unsummon

  /// ゲームの状態に関する条件
  type StaticCond =
    //| Not           of OEffectCond
    /// 属性共鳴 (自陣にこの属性のクリーチャーが3体存在すること)
    | Resonance     of Elem

  /// 単発的効果 (Oneshot Effect)
  type OEffect =
    | AsLongAs
      of StaticCond * then': OEffect * else': option<OEffect>
    | OEffectToUnits
      of OEffectToUnitType * Scope
    | Resurrect     of Amount
    | Swap          of ScopeForm
    | Rotate        of ScopeSide
    | GenToken      of list<CardSpec>

  and SkillAtom =
    SkillName * list<OEffect>

  /// 行動の効果 (Action Effect)
  and Skill =
    | SkillAtom     of SkillAtom
    | SkillList     of list<Skill>

  and TriggerCond =
    /// At the beginning of each turn
    | WhenBoT
    /// When entering the battlefield
    | WhenEtB
    | WhenDie
    //| WhenDealt

  and Ability =
    AbilName * (TriggerCond * list<OEffect>)

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
      Abils         : Map<TriggerCond, BatchedQueue<Ability>>
      Skills        : Map<Row, Skill>
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
      Status        : Status
      Effects       : list<KEffect>
    }

  type DeckSpec =
    {
      Name          : Name
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
      Name          : Name
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

  type Triggered =
    CardId * Place * Ability

  type Phase =
    | SummonPhase
    | UpkeepPhase
    | WindPhase     of blows: bool
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
    | PhaseBegin          of Phase
    | CardEnter           of CardId * Place
    | CardAbilityTrigger  of Triggered
    | SolveTriggered      of Triggered
    | CardBeginAction     of CardId * Skill
    | CardNullifyEffect   of CardId * OEffectToUnitType
    | CardIsCursed        of CardId * amount: int
    | CardHpInc           of CardId * amount: int
    | CardRegenerate      of CardId * amount: int
    | CardIsExiled        of CardId
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
      Triggered     : list<Triggered>

      Events        : Observable.Source<GameEvent * Game>
    }

  type GameEventStream =
    IObservable<GameEvent * Game>

  type GameLog =
    list<GameEvent * Game>

  type AbilitySrc =
    list<AbilName>

  /// ユーザが記述するカード仕様
  type CardSpecSrc =
    {
      Name          : CardName
      AT            : int
      AG            : int
      Elem          : string
      Abils         : AbilitySrc
      SkillFwd      : list<SkillName>
      SkillBwd      : list<SkillName>
    }

  type DeckSpecSrc =
    {
      Name          : Name
      Cards         : T7<CardSpecSrc>
    }

  [<Literal>]
  let StatusTotal = 300

  [<Literal>]
  let MaxDefaultAG = 50

  [<Literal>]
  let MaxTurns = 20
