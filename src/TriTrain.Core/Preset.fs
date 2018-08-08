namespace TriTrain.Core.Preset

open TriTrain.Core

module ScopeForm =
  let self    = ("自身", Self)

  let homeFwd = ("自陣前列", AbsForm (set [Fwd], Set.empty))
  let oppoFwd = ("敵陣前列", AbsForm (Set.empty, set [Fwd]))
  let homeBwd = ("自陣後列", AbsForm (set [Lft; Rgt], Set.empty))
  let oppoBwd = ("敵陣後列", AbsForm (Set.empty, set [Lft; Rgt]))
  let homeRgt = ("自陣右翼", AbsForm (set [Rgt], Set.empty))
  let oppoRgt = ("敵陣右翼", AbsForm (Set.empty, set [Rgt]))
  let homeAll = ("自陣全体", AbsForm (set (Vertex.all), Set.empty))
  let oppoAll = ("敵陣全体", AbsForm (Set.empty, set (Vertex.all)))

  let selfAndFwd =
    ("自身と前列", UnionForm [Self; homeFwd |> snd])

  let selfAndRgt =
    ("自身と右翼", UnionForm [Self; homeRgt |> snd])

  let bothAll =
    ("両陣全体", AbsForm (set Vertex.all, set Vertex.all))

module Scope =
  let self          = Scope.each ScopeForm.self
  let homeFwd       = Scope.each ScopeForm.homeFwd
  let homeBwdEach   = Scope.each ScopeForm.homeBwd
  let homeEach      = Scope.each ScopeForm.homeAll
  let oppoFwd       = Scope.each ScopeForm.oppoFwd
  let oppoBwdEach   = Scope.each ScopeForm.oppoBwd
  let oppoEach      = Scope.each ScopeForm.oppoAll
  let oppoRgt       = Scope.each ScopeForm.oppoRgt
  let bothEach      = Scope.each ScopeForm.bothAll

  let oppoMinHP     = Scope.minBy HP ScopeForm.oppoAll
  let oppoMaxAT     = Scope.maxBy AT ScopeForm.oppoAll

module KEffect =
  open KEffect

  let atInc rate = create (ATInc (AT, rate))
  let agInc rate = create (AGInc (AT, rate))

  let regenerate rate = create (Regenerate (AT, rate))

module Skill =
  open Scope
  open KEffect

  let attack rate scope =
    OEffectToUnits (Damage (AT, rate), scope)

  let heal rate scope =
    OEffectToUnits (Heal (AT, rate), scope)

  let hex rate scope =
    OEffectToUnits (Hex (AT, rate), scope)

  let sacrifice scope =
    OEffectToUnits (Hex (One, 100.0), scope)

  let give keff scope =
    OEffectToUnits (Give keff, scope)

  let cancel keffcan scope =
    OEffectToUnits (Cancel keffcan, scope)

  let resonance elem oeff =
    AsLongAs (Resonance elem, oeff, None)

  let presetList: list<SkillAtom> =
    [
      ("突撃"         , [attack 0.30 oppoFwd      ])
      ("薙ぎ払い"     , [attack 0.12 oppoBwdEach  ])
      ("狙撃"         , [attack 0.20 oppoRgt      ])
      ("大物狙い"     , [attack 0.24 oppoMaxAT    ])
      ("弱者狩り"     , [attack 0.20 oppoMinHP    ])

      ("呪詛"         , [hex 0.20 oppoFwd])
      ("禁呪"         , [hex 0.05 oppoEach])

      ("休息"         , [heal 0.40 self])
      ("治療"         , [heal 0.35 homeFwd])
      ("補給"         , [heal 0.12 homeEach])

      ("鍛錬"         , [give (atInc 0.50 2) self])
      ("支援"         , [give (atInc 0.45 2) homeFwd])
      ("鼓舞"         , [give (atInc 0.25 2) homeEach])

      ("飛翔"         , [give (agInc 0.50 2) self])
      ("天翔"         , [give (agInc 0.25 2) homeEach])

      ("仁王立ち"     , [Swap ScopeForm.selfAndFwd])
      ("退避"         , [Swap ScopeForm.selfAndRgt])

      ("堕落"         , [give (KEffect.create Damned 2) oppoEach])

      ("凪"           , [cancel AgIncCanceller bothEach])
      ( "幻痛"        , [cancel ImmuneCanceller oppoEach])

      ( "太陽破"
      , [sacrifice homeFwd; attack 0.20 oppoEach] )

      ( "激励"
      , [ give (agInc 0.15 2) homeEach
          give (atInc 0.15 2) homeEach ] )

      ( "照天"
      , [ give (agInc 0.30 3) homeEach
          give (atInc 0.30 3) homeEach
          give (KEffect.create Stable 2) homeEach
        ] )

      ("奇跡"         , [Resurrect (AT, 0.30)])

      ( "憑霊"
      , [sacrifice homeFwd; give (KEffect.create Haunted 3) self] )

      ( "烈風"        , [resonance Air   (give (agInc 0.40 2) homeEach)] )
      ( "烈火"        , [resonance Fire  (give (atInc 0.40 2) homeEach)] )
      ( "散水"        , [resonance Water (heal 0.30 homeEach)] )
      ( "救地"
      , [resonance Earth (give (KEffect.create (Regenerate (AT, 0.30)) 2) homeFwd)] )
    ]

  let preset: Map<Name, Skill> =
    presetList
    |> List.map (fun skillAtom -> (skillAtom |> fst, skillAtom |> SkillAtom))
    |> Map.ofList

  let internal presetOEffectsSet: Set<list<OEffect>> =
    preset
    |> Map.valueSet
    |> Set.map Skill.toEffectList

  /// プリセットに含まれる行動か？
  /// (名前は異なっていてもよい。)
  let isPreset (skill: Skill): bool =
    presetOEffectsSet |> Set.contains (skill |> Skill.toEffectList)

  /// プリセットに含まれる行動の組み合わせか？
  let isPresetList (skill: Skill): bool =
    let b1 = skill |> isPreset
    let b2 =
      match skill with
      | SkillAtom _ -> false
      | SkillList skills -> skills |> List.forall isPreset
    in b1 || b2

module Ability =
  open Scope
  open KEffect
  open Skill

  let presetList: list<Ability> =
    [
      ("晩成"         , (WhenBoT, [give (atInc 0.10 3) self]))
      ("不傷"         , (WhenEtB, [give (KEffect.create Immune 1) self]))
      ("恒常"         , (WhenEtB, [give (KEffect.create Stable 1) self]))
      ("神速"         , (WhenEtB, [give (KEffect.create (AGInc (One, 30.0)) 1) self]))
      ("怨念"         , (WhenDie, [give (KEffect.create (Curse (AT, 0.10)) 99) oppoEach]))
    ]

  let preset: Map<Name, Ability> =
    presetList
    |> List.map (fun ((name, _) as abil) -> (name, abil))
    |> Map.ofList

  let internal presetSet: Set<(TriggerCond * list<OEffect>)> =
    presetList |> List.map snd |> set

  let isPreset (abil: Ability): bool =
    presetSet |> Set.contains (abil |> snd)
