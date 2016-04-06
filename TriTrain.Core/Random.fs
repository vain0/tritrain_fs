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

  let skill () =
    Preset.Skill.presetList |> Random.element |> Option.get

  /// TODO: スキルの分配も乱択にする。能力に対応する。
  let cardSpec (): CardSpec =
    let skills =
      [ (FwdRow, skill ())
        (FwdRow, skill ())
        (BwdRow, skill ())
        (BwdRow, skill ())
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
