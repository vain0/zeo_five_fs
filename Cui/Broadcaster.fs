namespace ZeoFive.Cui

open ZeoFive.Core

module Broadcaster =
  let broadcaster obs =
    let f (g, g', ev) =
      match ev with
      | EvSummonSelect pl ->
          ()

      | EvSummon cardId ->
          let card = g |> Game.card cardId
          let pl   = g |> Game.player (card |> Card.owner)
          do
            printfn "Player %s summoned %s."
              (pl.Name) (card.Spec.Name)

      | EvCombat _ ->
          ()

      | EvAttackSelect (pl, way) ->
          let card = g |> Game.tryDohyoCard pl |> Option.get
          do
            printfn "%s attacked with %A."
              (card.Spec.Name) way

      | EvAttack pl ->
          ()

      | EvDamage (cardId, amount) ->
          let card  = g  |> Game.card cardId
          let card' = g' |> Game.card cardId
          do
            printfn "%s took %d damage. (HP %d -> %d)"
              (card.Spec.Name)
              amount
              (card  |> Card.curHp)
              (card' |> Card.curHp)

      | EvDie cardId ->
          let card = g |> Game.card cardId
          do
            printfn "%s died." (card.Spec.Name)

      | EvGameBegin ->
          do
            printfn "-- %s vs %s --"
              (g |> Game.player Player1).Name
              (g |> Game.player Player2).Name

      | EvGameEnd (Win plId) ->
          let pl = g |> Game.player plId
          do
            printfn "Player %s wins!!" (pl.Name)

      | EvGameEnd (Draw) ->
          printfn "Draw."
    in
      obs
      |> Observable.pairwise
      |> Observable.map (fun ((g, _), (g', ev)) -> (g, g', ev))
      |> Observable.subscribe f
