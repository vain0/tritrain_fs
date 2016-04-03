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

module KEffect =
  let atInc amount duration =
    {
      Type        = ATInc amount
      Duration    = Some duration
    }

  let agInc amount duration =
    {
      Type        = AGInc amount
      Duration    = Some duration
    }

module OEffect =
  open Scope
  open KEffect

  let pair oeff1 oeff2 =
    [ oeff1; oeff2 ] |> OEffectList

  let attack rate scope =
    OEffectToUnits (Damage (AT, rate), scope)

  let heal rate scope =
    OEffectToUnits (Heal (AT, rate), scope)

  let death rate scope =
    OEffectToUnits (Death (AT, rate), scope)

  let sacrifice scope =
    OEffectToUnits (Death (One, 100.0), scope)

  let give keff scope =
    OEffectToUnits (Give keff, scope)

  let preset =
    [
      attack 0.70 oppoFwd
      attack 0.30 oppoBwd
      attack 0.60 oppoRgt
      attack 0.20 oppoAll

      death 0.30 oppoFwd
      death 0.10 oppoAll

      heal 0.70 self
      heal 0.70 homeFwd
      heal 0.20 homeAll

      give (atInc (AT, 0.30) 2) self
      give (atInc (AT, 0.60) 1) homeFwd
      give (atInc (AT, 0.30) 2) homeFwd
      give (atInc (AT, 0.10) 2) homeAll

      give (agInc (AT, 0.30) 2) self
      give (agInc (AT, 0.30) 2) homeFwd
      give (agInc (AT, 0.15) 2) homeBwd
      give (agInc (AT, 0.10) 2) homeAll

      pair
        (sacrifice homeFwd)
        (attack 0.50 oppoAll)
      pair
        (death 0.50 self)
        (heal 0.40 homeAll)
      pair
        (give (atInc (AT, 0.05) 2) homeAll)
        (give (agInc (AT, 0.05) 2) homeAll)
    ]
    |> Set.ofList

  let isPreset oeff =
    preset |> Set.contains oeff
