namespace TriTrain.Core.Preset

open TriTrain.Core

module Scope =

  let self    = ("自身", Self)

  let homeFwd = ("自陣前列", AbsScope (set [Fwd], Set.empty))
  let oppoFwd = ("敵陣前列", AbsScope (Set.empty, set [Fwd]))
  let homeBwd = ("自陣後列", AbsScope (set [Lft; Rgt], Set.empty))
  let oppoBwd = ("敵陣後列", AbsScope (Set.empty, set [Lft; Rgt]))
  let homeRgt = ("自陣右翼", AbsScope (set [Rgt], Set.empty))
  let oppoRgt = ("敵陣右翼", AbsScope (Set.empty, set [Rgt]))
  let homeAll = ("自陣全体", AbsScope (set (Vertex.all), Set.empty))
  let oppoAll = ("敵陣全体", AbsScope (Set.empty, set (Vertex.all)))

  let selfAndFwd =
    ("自身と前列", UnionScope [self |> snd; homeFwd |> snd])

  let selfAndRgt =
    ("自身と右翼", UnionScope [self |> snd; homeRgt |> snd])

module KEffect =
  let createWithDuration typ duration =
    KEffect.create typ (Some duration)

  let atInc amount = createWithDuration (ATInc amount)
  let agInc amount = createWithDuration (AGInc amount)

  let regenerate rate = createWithDuration (Regenerate (AT, rate))

module OEffect =
  open Scope
  open KEffect

  let pair oeff1 oeff2 =
    [ oeff1; oeff2 ] |> OEffectList

  let attack rate scope =
    OEffectToUnits (Damage (AT, rate), scope) |> OEffectAtom

  let heal rate scope =
    OEffectToUnits (Heal (AT, rate), scope) |> OEffectAtom

  let death rate scope =
    OEffectToUnits (Death (AT, rate), scope) |> OEffectAtom

  let sacrifice scope =
    OEffectToUnits (Death (One, 100.0), scope) |> OEffectAtom

  let give keff scope =
    OEffectToUnits (Give keff, scope) |> OEffectAtom

  let presetList =
    [
      ("通常攻撃"     , attack 0.70 oppoFwd)
      ("後列薙ぎ"     , attack 0.30 oppoBwd)
      ("右翼の狙撃"   , attack 0.60 oppoRgt)
      ("全体攻撃"     , attack 0.20 oppoAll)

      ("ザキ"         , death 0.30 oppoFwd)
      ("ザラキ"       , death 0.10 oppoAll)

      ("自己回復"     , heal 0.70 self)
      ("前列回復"     , heal 0.70 homeFwd)
      ("全体回復"     , heal 0.20 homeAll)

      ("習熟"         , give (atInc (AT, 0.30) 2) self)
      ("協力"         , give (atInc (AT, 0.60) 1) homeFwd)
      ("支援"         , give (atInc (AT, 0.30) 2) homeFwd)
      ("鼓舞"         , give (atInc (AT, 0.10) 2) homeAll)

      ("飛翔"         , give (agInc (AT, 0.30) 2) self)
      ("送風"         , give (agInc (AT, 0.30) 2) homeFwd)
      ("旋風"         , give (agInc (AT, 0.15) 2) homeBwd)
      ("天翔"         , give (agInc (AT, 0.10) 2) homeAll)

      ("仁王立ち"     , Swap selfAndFwd |> OEffectAtom)
      ("退避"         , Swap selfAndRgt |> OEffectAtom)

      ("転生印"       , give (regenerate 0.50 1) homeFwd)

      ( "太陽破"
      , pair (sacrifice homeFwd) (attack 0.50 oppoAll) )

      ( "献身"
      , pair(death 0.50 self) (heal 0.40 homeAll) )

      ( "龍ノ舞"
      , pair
          (give (atInc (AT, 0.05) 2) homeAll)
          (give (agInc (AT, 0.05) 2) homeAll) )
    ]

  let preset =
    presetList
    |> List.map (fun (name, oeff) -> (name, (name, oeff)))
    |> Map.ofList

  let presetSet: Set<OEffect> =
    preset
    |> Map.valueSet
    |> Set.map snd  // discard names

  /// プリセットに含まれる効果か？
  /// (名前は異なっていてもよい。)
  let isPreset oeff =
    presetSet |> Set.contains oeff

  /// プリセットに含まれる効果の組み合わせか？
  let rec isPresetList oeff =
    let b1 = oeff |> isPreset
    let b2 =
      match oeff with
      | OEffectList oeffs -> oeffs |> List.forall isPresetList
      | OEffectAtom oeffa -> false
    in b1 || b2

  /// 効果 oeff を構成する部分効果のうち、プリセットであるもののリスト
  /// プリセットでないものが含まれているなら、それらは無視される。
  let rec toPresetList oeff: list<OEffect> =
    if oeff |> isPreset
    then
      [oeff]
    else
      match oeff with
      | OEffectList oeffs -> oeffs |> List.collect toPresetList
      | _ -> []

module Ability =
  open Scope
  open KEffect
  open OEffect

  let presetList: list<Ability> =
    [
      ("躍神"         , (WhenBoT, give (atInc (AT, 0.10) 3) self))
      ("神速"         , (WhenEtB, give (agInc (One, 30.0) 1) self))
    ]

  let preset =
    presetList
    |> List.map (fun ((name, _) as abil) -> (name, abil))
    |> Map.ofList

  let internal presetSet: Set<(TriggerCond * OEffect)> =
    presetList |> List.map snd |> set

  let isPreset (abil: Ability) =
    presetSet |> Set.contains (abil |> snd)
