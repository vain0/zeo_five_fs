namespace ZeoFive

open ZeoFive.Core

module Game =
  let updatePhase phase (g: Game) =
    { g with
        Phase = phase
      }

  let endWith r g =
    g |> updatePhase (PhGameEnd r)

  let updateDohyo pl cardId (g: Game) =
    { g with
        Dohyo =
          g.Dohyo |> Map.add pl cardId
      }

  let updateCard cardId card (g: Game) =
    assert (card.CardId = cardId)
    { g with
        Board =
          g.Board |> Map.add cardId card
      }

  let summonCard pl cardId (g: Game) =
    g
    |> Game.event (EvSummon cardId)
    |> updateDohyo pl cardId

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
        |> updatePhase PhCombat

  let dealDamage pl way (g: Game) =
    let plTarget        = pl |> Player.inverse
    let atk             = g |> Game.tryDohyoCard pl       |> Option.get
    let target          = g |> Game.tryDohyoCard plTarget |> Option.get
    let amount          = atk |> Card.power way
    let damage'         = target.Damage |> (+) amount |> min (target.Spec.Hp)
    let target          = { target with Damage = damage' }
    let g =
        g
        |> Game.event (EvDamage (target.CardId, amount))
        |> updateCard (target.CardId) target
    in
      // 死亡判定
      if target |> Card.curHp |> flip (<=) 0
      then
        g
        |> Game.event (EvDie target.CardId)
        |> updatePhase (PhSummon (target |> Card.owner))
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
      |> Game.event (EvAttack (pl, attackWay))
      |> dealDamage pl attackWay
      |> updateCard
          (attacker.CardId)
          { attacker with PrevWay = Some attackWay }

  let doAttackPhase order (g: Game) =
    match order with
    | [] ->
        g |> updatePhase PhCombat
    | pl :: rest ->
        g
        |> updatePhase (PhAttack rest)
        |> attack pl

  let sortBySpeed (g: Game) =
    g.Dohyo
    |> Map.toList
    |> List.map snd
    |> List.sortBy (fun cardId ->
        (g |> Game.card cardId).Spec.Spd
        )
    |> List.rev

  let doCombatPhase (g: Game) =
    let order =
      g
      |> sortBySpeed
      |> List.map (fun cardId ->
          g |> Game.card cardId |> Card.owner
          )
    in
      g |> updatePhase (PhAttack order)

  let doBeginPhase (g: Game) =
    g
    |> Game.event EvGameBegin
    |> doSummonPhase Player1
    |> doSummonPhase Player2
    
  let rec doPhase (g: Game) =
    match g.Phase with
    | PhGameEnd r ->
        let g = g |> Game.event (EvGameEnd r)
        in (g, r)
    | PhGameBegin ->
        g |> doBeginPhase |> doPhase
    | PhSummon pl ->
        g |> doSummonPhase pl |> doPhase
    | PhCombat ->
        g |> doCombatPhase |> doPhase
    | PhAttack order ->
        g |> doAttackPhase order |> doPhase

  let play audience pl1 pl2 =
    (pl1, pl2)
    ||> Game.init audience
    |> doPhase
