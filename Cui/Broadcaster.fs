namespace ZeoFive.Cui

open ZeoFive.Core

module Broadcaster =
  let broadcaster obs =
    let f (g, g', ev) =
      match ev with
      | EvSummonSelect pl ->
          ()

      | EvSummon cardId ->
          let card = g |> UpdateCont.eval (Game.getCard cardId)
          let pl   = g |> UpdateCont.eval (Game.getPlayer (card |> Card.owner))
          do
            printfn "Player %s summoned %s."
              (pl.Name) (card.Spec.Name)

      | EvCombat _ ->
          ()

      | EvAttackSelect (pl, way) ->
          let card = g |> UpdateCont.eval (Game.getDohyoCard pl)
          do
            printfn "%s attacked with %A."
              (card.Spec.Name) way

      | EvAttack pl ->
          ()

      | EvDamage (cardId, amount) ->
          let card  = g  |> UpdateCont.eval (Game.getCard cardId)
          let card' = g' |> UpdateCont.eval (Game.getCard cardId)
          do
            printfn "%s took %d damage. (HP %d -> %d)"
              (card.Spec.Name)
              amount
              (card  |> Card.curHp)
              (card' |> Card.curHp)

      | EvDie cardId ->
          let card = g |> UpdateCont.eval (Game.getCard cardId)
          do
            printfn "%s died." (card.Spec.Name)

      | EvGameBegin ->
          let plName plId =
            (g |> UpdateCont.eval (Game.getPlayer plId)).Name
          do
            printfn "-- %s vs %s --"
              (plName Player1)
              (plName Player2)

      | EvGameEnd (Win plId) ->
          let pl = g |> UpdateCont.eval (Game.getPlayer plId)
          do
            printfn "Player %s wins!!" (pl.Name)

      | EvGameEnd (Draw) ->
          printfn "Draw."
    in
      obs
      |> Observable.duplicateFirst  // duplicate `EvGameBegin` event
      |> Observable.pairwise
      |> Observable.map (fun ((g, _), (g', ev)) -> (g, g', ev))
      |> Observable.subscribe f
