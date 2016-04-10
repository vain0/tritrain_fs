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

module Scope =
  let self          = Scope.each ScopeForm.self
  let homeFwd       = Scope.each ScopeForm.homeFwd
  let homeBwdEach   = Scope.each ScopeForm.homeBwd
  let homeEach      = Scope.each ScopeForm.homeAll
  let oppoFwd       = Scope.each ScopeForm.oppoFwd
  let oppoBwdEach   = Scope.each ScopeForm.oppoBwd
  let oppoEach      = Scope.each ScopeForm.oppoAll
  let oppoRgt       = Scope.each ScopeForm.oppoRgt

  let oppoMinHP     = Scope.minBy HP ScopeForm.oppoAll
  let oppoMaxAT     = Scope.maxBy AT ScopeForm.oppoAll

module KEffect =
  open KEffect

  let atInc amount = create (ATInc amount)
  let agInc amount = create (AGInc amount)

  let regenerate rate = create (Regenerate (AT, rate))

module Skill =
  open Scope
  open KEffect

  let pair oeffs1 oeffs2 =
    oeffs1 @ oeffs2

  let attack rate scope =
    [OEffectToUnits (Damage (AT, rate), scope)]

  let heal rate scope =
    [OEffectToUnits (Heal (AT, rate), scope)]

  let death rate scope =
    [OEffectToUnits (Death (AT, rate), scope)]

  let sacrifice scope =
    [OEffectToUnits (Death (One, 100.0), scope)]

  let give keff scope =
    [OEffectToUnits (Give keff, scope)]

  let presetList: list<SkillAtom> =
    [
      ("通常攻撃"     , attack 0.70 oppoFwd)
      ("後列薙ぎ"     , attack 0.30 oppoBwdEach)
      ("右翼の狙撃"   , attack 0.60 oppoRgt)
      ("全体攻撃"     , attack 0.20 oppoEach)
      ("大物狙い"     , attack 0.60 oppoMaxAT)
      ("弱者狩り"     , attack 0.50 oppoMinHP)

      ("ザキ"         , death 0.30 oppoFwd)
      ("ザラキ"       , death 0.10 oppoEach)

      ("自己回復"     , heal 0.70 self)
      ("前列回復"     , heal 0.70 homeFwd)
      ("全体回復"     , heal 0.20 homeEach)

      ("習熟"         , give (atInc (AT, 0.30) 2) self)
      ("協力"         , give (atInc (AT, 0.60) 1) homeFwd)
      ("支援"         , give (atInc (AT, 0.30) 2) homeFwd)
      ("鼓舞"         , give (atInc (AT, 0.10) 2) homeEach)

      ("飛翔"         , give (agInc (AT, 0.30) 2) self)
      ("送風"         , give (agInc (AT, 0.30) 2) homeFwd)
      ("旋風"         , give (agInc (AT, 0.15) 2) homeBwdEach)
      ("天翔"         , give (agInc (AT, 0.10) 2) homeEach)

      ("仁王立ち"     , [Swap ScopeForm.selfAndFwd])
      ("退避"         , [Swap ScopeForm.selfAndRgt])

      ("転生印"       , give (regenerate 0.50 1) homeFwd)

      ("堕落"         , give (KEffect.create Damned 2) oppoEach)

      ( "太陽破"
      , pair (sacrifice homeFwd) (attack 0.50 oppoEach) )

      ( "献身"
      , pair(death 0.50 self) (heal 0.40 homeEach) )

      ( "龍ノ舞"
      , pair
          (give (atInc (AT, 0.05) 2) homeEach)
          (give (agInc (AT, 0.05) 2) homeEach) )

      ( "照天"
      , [ yield! give (agInc (AT, 0.30) 3) homeEach
          yield! give (atInc (AT, 0.30) 3) homeEach
          yield! give (KEffect.create Stable 2) homeEach
        ] )

      ("奇跡"
      , pair
          [Resurrect (AT, 0.30)]
          (give (agInc (AT, -0.30) 2) homeEach) )
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
      ("躍神"         , (WhenBoT, give (atInc (AT, 0.10) 3) self))
      ("不傷"         , (WhenEtB, give (KEffect.create Immune 1) self))
      ("恒常"         , (WhenEtB, give (KEffect.create Stable 1) self))
      ("神速"         , (WhenEtB, give (agInc (One, 30.0) 1) self))
    ]

  let preset: Map<Name, Ability> =
    presetList
    |> List.map (fun ((name, _) as abil) -> (name, abil))
    |> Map.ofList

  let internal presetSet: Set<(TriggerCond * list<OEffect>)> =
    presetList |> List.map snd |> set

  let isPreset (abil: Ability): bool =
    presetSet |> Set.contains (abil |> snd)
