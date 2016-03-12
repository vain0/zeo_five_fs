namespace ZeoFive

open ZeoFive.Core

module Game =
  let doSummonSelectEvent plId (g: Game) =
    // 全滅判定
    if (g |> Game.player plId).Hand |> List.isEmpty
    then
      g |> Game.endWith (plId |> Player.inverse |> Win)
    else
      let brain     = (g |> Game.player plId).Brain
      let state     =  g |> Game.state plId
      let cardId    = brain.Summon(state)
      in
        assert (g |> Game.card cardId |> Card.owner |> (=) plId)
      ; g |> Game.happen (EvSummon (cardId))
        
  let nextActor actedPls (g: Game) =
      g
      |> Game.dohyoCards
      |> Set.toList
      |> List.filter (fun (plId, _) ->
          actedPls |> Set.contains plId |> not
          )
      |> List.tryMaxBy
          (fun cardId -> (g |> Game.card cardId).Spec.Spd)
      |> Option.map fst

  let doCombatEvent actedPls g =
    assert (g |> Game.dohyoCards |> Set.count |> (=) 2)
    match g |> nextActor actedPls with
    | None ->
        g |> Game.happen (EvCombat Set.empty)
    | Some actor ->
        g
        |> Game.happen (EvCombat (actedPls |> Set.add actor))
        |> Game.happen (EvAttackSelect actor)

  let attackSelect plId (g: Game) =
    let attacker =
      g |> Game.tryDohyoCard plId |> Option.get
    let attackWay =
      match attacker.PrevWay with
      | Some prev ->
          prev |> AttackWay.inverse
      | None ->
        let brain = (g |> Game.player plId).Brain
        in
          brain.Attack(g |> Game.state plId)
    in
      (g, attackWay)

  let doAttackSelectEvent plId g =
    let (g, attackWay) =
        g |> attackSelect plId
    let attacker =
        g |> Game.tryDohyoCard plId |> Option.get
    in
      g
      |> Game.updateCard
          attacker.CardId
          { attacker with PrevWay = Some attackWay }
      |> Game.happen (EvAttack (plId, attackWay))

  let doAttackEvent (plId, attackWay) g =
    let plTarget  = plId |> Player.inverse
    let attacker  = g |> Game.tryDohyoCard plId     |> Option.get
    let target    = g |> Game.tryDohyoCard plTarget |> Option.get
    let amount    =
      attacker
      |> Card.power attackWay
      |> min (target |> Card.curHp)
    in
      g |> Game.happen (EvDamage (target.CardId, amount))

  let doDamageEvent (cardId, amount) g =
    let card    = g |> Game.card cardId
    let card    = { card with Damage = card.Damage + amount }
    let g       = g |> Game.updateCard cardId card
    in
      // 死亡判定
      if (card |> Card.curHp) <= 0
      then g |> Game.happen (EvDie cardId)
      else g

  let doEvent ev (g: Game) =
      match ev with
      | EvSummonSelect plId ->
          g |> doSummonSelectEvent plId

      | EvSummon cardId ->
          g
          |> Game.updateDohyo (fst cardId) cardId
          |> Game.updateHand (fst cardId) (List.filter ((<>) cardId))

      | EvCombat actedPls ->
          g |> doCombatEvent actedPls

      | EvAttackSelect plId ->
          g |> doAttackSelectEvent plId

      | EvAttack (plId, attackWay) ->
          g |> doAttackEvent (plId, attackWay)

      | EvDamage (cardId, amount) ->
          g |> doDamageEvent (cardId, amount)

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

  let rec doNextEvent audience (g: Game) =
    let rec loop g =
      match g.Kont with
      | [] -> failwith "game stuck"
      | ev :: kont ->
          let g' =
            { g with Kont = kont }
            |> doEvent ev
          let () =
            audience
            |> List.iter (fun (lis: IListener) -> lis.Listen(g, g', ev))
          in
            match ev with
            | EvGameEnd r -> (g', r)
            | _ ->
                g' |> loop
    in
      loop g

  let play audience pl1 pl2 =
    (pl1, pl2)
    ||> Game.init
    |> doNextEvent audience
