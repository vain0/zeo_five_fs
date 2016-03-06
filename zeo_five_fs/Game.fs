namespace ZeoFive

open ZeoFive.Core

module Game =
  let updatePhase phase (g: Game) =
    { g with
        Phase = phase
      }

  let endWith r g =
    g |> updatePhase (GameEnd r)

  let updateBattlefield side cardId prev (g: Game) =
    { g with
        Battlefield =
          g.Battlefield |> Map.add side (cardId, prev)
      }

  let updateCard cardId card (g: Game) =
    assert (card.CardId = cardId)
    { g with
        Board =
          g.Board |> Map.add cardId card
      }

  let summonCard side cardId (g: Game) =
    printfn "Player %A summoned %s."
      (g |> Game.playerOn side)
      ((g.Board |> Map.find cardId).Spec.Name)

    g |> updateBattlefield side cardId None

  let doSummonPhase side (g: Game) =
    let brain =
      (g |> Game.playerOn side).Brain
    let state =
      g |> Game.state side
    in
      // 全滅判定
      if state.Board |> Map.forall (fun _ -> Card.isDead)
      then
        g |> endWith (side |> PlayerSide.inverse |> Win)
      else
        g 
        |> summonCard side (brain.Summon(state))
        |> updatePhase CombatPhase

  let dealDamage side (g: Game) =
    let (attackerId, attackWayOpt) =
      g.Battlefield |> Map.find side
    let attacker =
      g.Board |> Map.find attackerId
    let (targetId, _) =
      g.Battlefield |> Map.find (side |> PlayerSide.inverse)
    let target =
      g.Board |> Map.find targetId
    let attackWay =
      attackWayOpt |> Option.get
    let amount =
      attacker |> Card.power attackWay
    let target =
      { target 
          with Damage = target.Damage |> (+) amount |> min (target.Spec.Hp) }
    let g =
      { g with
          Board = g.Board |> Map.add targetId target }
    in
      // 死亡判定
      if target |> Card.curHp |> flip (<=) 0
      then
        printfn "%s died." (target.Spec.Name)
        g |> updatePhase (SummonPhase (side |> PlayerSide.inverse))
      else
        g

  let selectAttackWay side way (g: Game) =
    let (cardId, _) =
      g.Battlefield |> Map.find side
    in
      g |> updateBattlefield side cardId (Some way)

  let attack side (g: Game) =
    let (cardId, prev) =
      assert (g.Battlefield |> Map.containsKey side)
      g.Battlefield |> Map.find side 
    let attackWay =
      match prev with
      | Some prev ->
          prev |> AttackWay.inverse
      | None ->
        let brain =
          (g |> Game.playerOn side).Brain
        in
          brain.Attack(g |> Game.state side)
    in
      g
      |> selectAttackWay side attackWay
      |> dealDamage side

  let doAttackPhase order (g: Game) =
    match order with
    | [] ->
        g |> updatePhase CombatPhase
    | side :: rest ->
        g
        |> updatePhase (AttackPhase rest)
        |> attack side

  let sortBySpeed (g: Game) =
    g.Battlefield
    |> Map.toList
    |> List.map (fun (_, (cardId, _)) -> cardId)
    |> List.sortBy (fun cardId ->
        (g.Board |> Map.find cardId).Spec.Spd
        )
    |> List.rev

  let doCombatPhase (g: Game) =
    let order =
      g
      |> sortBySpeed
      |> List.map (fun cardId ->
          (g.Board |> Map.find cardId).Side
          )
    in
      g |> updatePhase (AttackPhase order)

  let doBeginPhase (g: Game) =
    g
    |> doSummonPhase First
    |> doSummonPhase Second
    
  let rec doPhase (g: Game) =
    match g.Phase with
    | GameEnd r ->
        (g, r)
    | GameBegin ->
        g |> doBeginPhase |> doPhase
    | SummonPhase side ->
        g |> doSummonPhase side |> doPhase
    | CombatPhase ->
        g |> doCombatPhase |> doPhase
    | AttackPhase order ->
        g |> doAttackPhase order |> doPhase

  let play pl1 pl2 =
    let (g, result) =
      (pl1, pl2)
      ||> Game.init
      |> doPhase
    in
      result
