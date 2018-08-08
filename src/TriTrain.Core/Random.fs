namespace TriTrain.Core

module Random =
  let name () =
    sprintf "'%.5f'" (Random.rng.NextDouble())

  let elem () =
    Elem.all |> Random.element |> Option.get

  let ag () =
    Random.rng.Next(0, 50)

  let status () =
    let ag = ag ()
    let at = Random.rng.Next(0, StatusTotal - ag)
    in Status.ofAtAg at ag

  let abil () =
    Preset.Ability.presetList |> Random.element |> Option.get

  let skillAtom () =
    Preset.Skill.presetList |> Random.element |> Option.get

  /// n 個の基本行動からなる行動
  let skillList n =
    List.init n (fun _ -> skillAtom ())
    |> List.map SkillAtom
    |> SkillList

  /// TODO: スキルの分配も乱択にする。能力に対応する。
  let cardSpec (): CardSpec =
    let skills =
      [ (FwdRow, skillList 2)
        (BwdRow, skillList 2)
      ]
      |> Map.ofList
    let abils =
      Map.empty
    in
      {
        Name          = name ()
        Status        = status ()
        Elem          = elem ()
        Abils         = abils
        Skills        = skills
      }

  let deckSpec (): DeckSpec =
    {
      Name      = name ()
      Cards     = T7.init (fun _ -> cardSpec ())
    }

  /// ATの値を 3:1 の比率で合成する
  let altStatus status statusAlt =
    let at =
      ((status |> Status.at) * 3 + (statusAlt |> Status.at)) / 4
    in Status.ofAtAg at (status |> Status.ag)

  /// 前列・後列のどちらかの行動を差し替える
  let altSkills skills skillsAlt =
      match skillsAlt |> Map.toList |> Random.element with
      | None -> skills
      | Some (row, skill) ->
          skills |> Map.add row skill

  let altCardSpec (cspec: CardSpec) (cspecAlt: CardSpec): CardSpec =
    let cspec = { cspec with Skills = altSkills cspec.Skills cspecAlt.Skills }
    let cspec = { cspec with Status = altStatus cspec.Status cspecAlt.Status }
    in cspec
