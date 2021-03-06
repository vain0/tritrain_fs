namespace TriTrain.Core

open TriTrain.Core
open System

// TODO: internationalization
module Dump =
  let dumpElem =
    function
    | Air   -> "風"
    | Fire  -> "火"
    | Water -> "水"
    | Earth -> "地"

  let dumpRow =
    function
    | FwdRow -> "前列"
    | BwdRow -> "後列"

  let dumpVarType =
    function
    | MaxHP -> "最大HP"
    | HP -> "HP"
    | AT -> "AT"
    | AG -> "AG"
    | One -> "1"  // never

  let dumpAmount (var, rate) =
    match var with
    | One -> rate |> int |> string
    | var -> dumpVarType var + "×" + string rate

  let dumpScopeSide =
    function
    | Home -> "自陣"
    | Oppo -> "敵陣"
    | Both -> "両陣"

  let dumpScopeForm form =
    form |> ScopeForm.name

  let dumpScope scope =
    let form = scope |> Scope.form |> dumpScopeForm
    in
      match scope |> Scope.aggregate with
      | Each ->
          if (scope |> Scope.form |> ScopeForm.maxSize) = 1
          then form
          else form + "の各クリーチャー"
      | MaxBy (var, rev) ->
          sprintf "%sで%sが%sのもの"
            form
            (dumpVarType var)
            (if rev then "最小" else "最大")

  let dumpCond =
    function
    | WhenBoT -> "各ターン開始時"
    | WhenEtB -> "盤面に出たとき"
    | WhenDie -> "死亡したとき"

  let dumpStaticCond =
    function
    | Resonance elem ->
        sprintf "自陣に%s属性クリーチャーが3体いる場合" (elem |> dumpElem)

  let dumpKEffectType =
    function
    | ATInc amount ->
        sprintf "ATが%s点増加する効果"
          (dumpAmount amount)
    | AGInc amount ->
        sprintf "AGが%s点増加する効果"
          (dumpAmount amount)
    | Regenerate amount ->
        sprintf "死亡後に最大HPの%s%cで再生する効果"
          (dumpAmount amount) '%'
    | Curse amount ->
        sprintf "毎ターン%s点のダメージを受ける呪いの効果"
          (dumpAmount amount)
    | Immune ->
        "ダメージを受けない効果"
    | Stable ->
        "継続的効果を受けない効果"
    | Damned ->
        "死亡後に追放される効果"
    | Haunted ->
        "憑霊効果"

  let dumpKEffect keff =
    let dur = sprintf "(%dT)" (keff |> KEffect.duration)
    let typ = keff |> KEffect.typ |> dumpKEffectType
    in typ + dur

  /// これが打ち消せる効果を表す語句
  let dumpKEffectCanceller =
    function
    | AgIncCanceller -> "AGが増減する効果"
    | CurseCanceller  -> "呪いの効果"
    | ImmuneCanceller -> dumpKEffectType Immune

  let dumpOEffectToUnit (typ, scope) =
    match typ with
    | Damage amount ->
        sprintf "%sに%s点のダメージを与える。"
          (dumpScope scope)
          (dumpAmount amount)
    | Heal amount ->
        sprintf "%sを%s点回復する。"
          (dumpScope scope)
          (dumpAmount amount)
    | Hex amount ->
        sprintf "%sを%s%cの確率で即死させる。"
          (dumpScope scope)
          (dumpAmount amount)
          '%'
    | Give keff ->
        sprintf "%sに%sを与える。"
          (dumpScope scope)
          (dumpKEffect keff)
    | Cancel keffcan ->
        sprintf "%sにかかっている%sを打ち消す。"
          (dumpScope scope)
          (dumpKEffectCanceller keffcan)

  let rec dumpOEffect oeff =
    match oeff with
    | AsLongAs (cond, then', else') ->
        sprintf "%s、%s%s"
          (cond |> dumpStaticCond)
          (then' |> dumpOEffect)
          (match else' with
            | None -> ""
            | Some oeff -> sprintf "そうでなければ、%s" (oeff |> dumpOEffect)
            )

    | OEffectToUnits (typ, scope) ->
        dumpOEffectToUnit (typ, scope)
    | Resurrect amount ->
        sprintf "味方1体を最大HPの%s%c持った状態で蘇生する。"
          (dumpAmount amount) '%'
    | Swap form ->
        dumpScopeForm form + "を交代する。"
    | Rotate scopeSide ->
        dumpScopeSide scopeSide + "の回転を起こす。"
    | GenToken cards ->
        failwith "unimplemented"

  and dumpOEffectList (oeffs: list<OEffect>) =
    oeffs
    |> List.map dumpOEffect
    |> String.concat ""

  and dumpStatus st =
    sprintf "HP%d/AT%d/AG%d"
      (st |> Status.hp)
      (st |> Status.at)
      (st |> Status.ag)

  and dumpSkillOf row skills =
    match skills |> Map.tryFind row with
    | Some skill ->
        sprintf "{%s「%s」: %s}"
          (row |> dumpRow)
          (skill |> Skill.name)
          (skill |> Skill.toEffectList |> dumpOEffectList)
    | None -> ""

  and dumpCardSpec spec =
    sprintf "《%s》(%s)%s%s"
      (spec |> CardSpec.name)
      (spec |> CardSpec.status |> dumpStatus)
      (spec |> CardSpec.skills |> dumpSkillOf FwdRow)
      (spec |> CardSpec.skills |> dumpSkillOf BwdRow)

  let dumpPresetSkillsToMarkdown () =
    let rows =
      Preset.Skill.presetList
      |> List.map (fun (name, oeffs) ->
          sprintf "|%s|%s|" name (oeffs |> dumpOEffectList)
          )
      |> String.concat (Environment.NewLine)
    let header =
        """
## 行動(Skills)
|Name|Text|
|:---|:---|
"""
    in header + rows

  let dumpPresetAbilsToMarkdown () =
    let rows =
      Preset.Ability.presetList
      |> List.map (fun (name, (cond, oeffs)) ->
          sprintf "|%s|%s|%s|"
            name
            (cond |> dumpCond)
            (oeffs |> dumpOEffectList)
          )
      |> String.concat (Environment.NewLine)
    let header =
        """
## 能力(Abilities)
|Name|Cond|Text|
|:---|:---|:---|
"""
    in header + rows
