namespace ZeoFive

open ZeoFive.Core

module Game =
  let doSummonSelectEvent pl (g: Game) =
    let brain = (g |> Game.player pl).Brain
    let state =  g |> Game.state pl
    in
      // 全滅判定
      if state.Board |> Map.forall (fun _ -> Card.isDead)
      then
        g |> Game.endWith (pl |> Player.inverse |> Win)
      else
        g |> Game.happen (EvSummon (brain.Summon(pl, state)))

  let attackSelect pl (g: Game) =
    let attacker =
      g |> Game.tryDohyoCard pl |> Option.get
    let attackWay =
      match attacker.PrevWay with
      | Some prev ->
          prev |> AttackWay.inverse
      | None ->
        let brain = (g |> Game.player pl).Brain
        in
          brain.Attack(pl, g |> Game.state pl)
    in
      (g, attackWay)

  let doAttackSelectEvent pl g =
    let (g, attackWay) =
        g |> attackSelect pl
    let attacker =
        g |> Game.tryDohyoCard pl |> Option.get
    in
      g
      |> Game.updateCard
          attacker.CardId
          { attacker with PrevWay = Some attackWay }
      |> Game.happen (EvAttack (pl, attackWay))

  let nextActor actedPls (g: Game) =
      g.Dohyo
      |> Map.toList
      |> List.filter (fun (pl, _) ->
          actedPls |> Set.contains pl |> not
          )
      |> List.tryMaxBy
          (fun (pl, cardId) -> (g |> Game.card cardId).Spec.Spd)
      |> Option.map fst

  let doEvent ev (g: Game) =
      match ev with
      | EvSummonSelect pl ->
          g |> doSummonSelectEvent pl

      | EvSummon cardId ->
          g |> Game.updateDohyo (fst cardId) cardId

      | EvCombat actedPls ->
          match g |> nextActor actedPls with
          | None ->
              g |> Game.happen (EvCombat Set.empty)
          | Some actor ->
              g
              |> Game.happen (EvCombat (actedPls |> Set.add actor))
              |> Game.happen (EvAttackSelect actor)

      | EvAttackSelect pl ->
          g |> doAttackSelectEvent pl

      | EvAttack (pl, attackWay) ->
          let plTarget  = pl |> Player.inverse
          let attacker  = g |> Game.tryDohyoCard pl       |> Option.get
          let target    = g |> Game.tryDohyoCard plTarget |> Option.get
          let amount    =
            attacker
            |> Card.power attackWay
            |> min (target |> Card.curHp)
          in
            g |> Game.happen (EvDamage (target.CardId, amount))

      | EvDamage (cardId, amount) ->
          let card    = g |> Game.card cardId
          let card    = { card with Damage = card.Damage + amount }
          let g       = g |> Game.updateCard cardId card
          in
            // 死亡判定
            if (card |> Card.curHp) <= 0
            then g |> Game.happen (EvDie cardId)
            else g

      | EvDie cardId ->
          { g with Kont = [] }
          |> Game.happen (EvCombat Set.empty)
          |> Game.happen (EvSummonSelect (fst cardId))

      | EvGameBegin ->
          g
          |> Game.happen (EvCombat Set.empty)
          |> Game.happen (EvSummonSelect Player2)
          |> Game.happen (EvSummonSelect Player1)

      | EvGameEnd _ ->
          g

  let rec doNextEvent (g: Game) =
    match g.Kont with
    | [] -> failwith "game stuck"
    | ev :: kont ->
        let g' =
          { g with Kont = kont }
          |> doEvent ev
        let () =
          g.Audience
          |> List.iter (fun lis -> lis.Listen(g, g', ev))
        in
          match ev with
          | EvGameEnd r -> (g', r)
          | _ ->
              g' |> doNextEvent

  let play audience pl1 pl2 =
    (pl1, pl2)
    ||> Game.init audience
    |> doNextEvent
