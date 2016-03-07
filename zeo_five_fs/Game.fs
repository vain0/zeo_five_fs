namespace ZeoFive

open ZeoFive.Core

module Game =
  let event ev (g: Game) =
    do
      g.Audience
      |> List.iter (fun lis -> lis.Listen(g, ev))
    g
    
  let endWith r g =
    g |> Game.updatePhase (PhGameEnd r)

  let beginCombat g =
    g |> Game.updatePhase (PhCombat Set.empty)

  let summonCard pl cardId (g: Game) =
    g
    |> event (EvSummon cardId)
    |> Game.updateDohyo pl cardId

  let doSummonPhase pl (g: Game) =
    let brain =
      (g |> Game.player pl).Brain
    let state =
      g |> Game.state pl
    in
      // 全滅判定
      if state.Board |> Map.forall (fun _ -> Card.isDead)
      then
        g |> endWith (pl |> Player.inverse |> Win)
      else
        g 
        |> summonCard pl (brain.Summon(pl, state))
        |> beginCombat

  let dealDamage pl way (g: Game) =
    let plTarget        = pl |> Player.inverse
    let atk             = g |> Game.tryDohyoCard pl       |> Option.get
    let target          = g |> Game.tryDohyoCard plTarget |> Option.get
    let amount          = atk |> Card.power way
    let damage'         = target.Damage |> (+) amount |> min (target.Spec.Hp)
    let target          = { target with Damage = damage' }
    let g =
        g
        |> event (EvDamage (target.CardId, amount))
        |> Game.updateCard (target.CardId) target
    in
      // 死亡判定
      if target |> Card.curHp |> flip (<=) 0
      then
        g
        |> event (EvDie target.CardId)
        |> Game.updatePhase (PhSummon (target |> Card.owner))
      else
        g

  let attack pl (g: Game) =
    let attacker =
      g |> Game.tryDohyoCard pl |> Option.get
    let attackWay =
      match attacker.PrevWay with
      | Some prev ->
          prev |> AttackWay.inverse
      | None ->
        let brain =
          (g |> Game.player pl).Brain
        in
          brain.Attack(pl, g |> Game.state pl)
    in
      g
      |> event (EvAttack (pl, attackWay))
      |> dealDamage pl attackWay
      |> Game.updateCard
          (attacker.CardId)
          { attacker with PrevWay = Some attackWay }

  let nextActor actedPls (g: Game) =
      g.Dohyo
      |> Map.toList
      |> List.filter (fun (pl, _) ->
          actedPls |> Set.contains pl |> not
          )
      |> List.tryMaxBy
          (fun (pl, cardId) -> (g |> Game.card cardId).Spec.Spd)
      |> Option.map fst

  let doCombatPhase actedPls (g: Game) =
    match g |> nextActor actedPls with
    | None ->
        g |> beginCombat
    | Some actor ->
        g
        |> Game.updatePhase (PhCombat (actedPls |> Set.add actor))
        |> attack actor

  let doBeginPhase (g: Game) =
    g
    |> event EvGameBegin
    |> doSummonPhase Player1
    |> doSummonPhase Player2
    
  let rec doPhase (g: Game) =
    match g.Phase with
    | PhGameEnd r ->
        let g = g |> event (EvGameEnd r)
        in (g, r)
    | PhGameBegin ->
        g |> doBeginPhase |> doPhase
    | PhSummon pl ->
        g |> doSummonPhase pl |> doPhase
    | PhCombat actedPls ->
        g |> doCombatPhase actedPls |> doPhase

  let play audience pl1 pl2 =
    (pl1, pl2)
    ||> Game.init audience
    |> doPhase
