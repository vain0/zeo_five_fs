namespace ZeoFive

open ZeoFive.Core

module Game =
  let updatePhase phase (g: Game) =
    { g with
        Phase = phase
      }

  let endWith r g =
    g |> updatePhase (GameEnd r)

  let updateDohyo pl cardId prev (g: Game) =
    { g with
        Dohyo =
          g.Dohyo |> Map.add pl (cardId, prev)
      }

  let updateCard cardId card (g: Game) =
    assert (card.CardId = cardId)
    { g with
        Board =
          g.Board |> Map.add cardId card
      }

  let summonCard pl cardId (g: Game) =
    printfn "Player %s summoned %s."
      ((g |> Game.player pl).Name)
      ((g |> Game.card cardId).Spec.Name)

    g |> updateDohyo pl cardId None

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
        |> summonCard pl (brain.Summon(state))
        |> updatePhase CombatPhase

  let dealDamage pl (g: Game) =
    let (atkId, wayOpt) = g.Dohyo |> Map.find pl
    let atk             = g |> Game.card atkId
    let (targetId, _)   = g.Dohyo |> Map.find (pl |> Player.inverse)
    let target          = g |> Game.card targetId
    let way             = wayOpt |> Option.get
    let amount          = atk |> Card.power way
    let damage'         = target.Damage |> (+) amount |> min (target.Spec.Hp)
    let target          = { target with Damage = damage' }
    let g               = g |> updateCard targetId target
    in
      // 死亡判定
      if target |> Card.curHp |> flip (<=) 0
      then
        printfn "%s died." (target.Spec.Name)
        g |> updatePhase (SummonPhase target.Owner)
      else
        g

  let selectAttackWay pl way (g: Game) =
    let (cardId, _) =
      g.Dohyo |> Map.find pl
    in
      g |> updateDohyo pl cardId (Some way)

  let attack pl (g: Game) =
    let (cardId, prev) =
      assert (g.Dohyo |> Map.containsKey pl)
      g.Dohyo |> Map.find pl 
    let attackWay =
      match prev with
      | Some prev ->
          prev |> AttackWay.inverse
      | None ->
        let brain =
          (g |> Game.player pl).Brain
        in
          brain.Attack(g |> Game.state pl)
    in
      g
      |> selectAttackWay pl attackWay
      |> dealDamage pl

  let doAttackPhase order (g: Game) =
    match order with
    | [] ->
        g |> updatePhase CombatPhase
    | pl :: rest ->
        g
        |> updatePhase (AttackPhase rest)
        |> attack pl

  let sortBySpeed (g: Game) =
    g.Dohyo
    |> Map.toList
    |> List.map (fun (_, (cardId, _)) -> cardId)
    |> List.sortBy (fun cardId ->
        (g |> Game.card cardId).Spec.Spd
        )
    |> List.rev

  let doCombatPhase (g: Game) =
    let order =
      g
      |> sortBySpeed
      |> List.map (fun cardId ->
          (g |> Game.card cardId).Owner
          )
    in
      g |> updatePhase (AttackPhase order)

  let doBeginPhase (g: Game) =
    g
    |> doSummonPhase Player1
    |> doSummonPhase Player2
    
  let rec doPhase (g: Game) =
    match g.Phase with
    | GameEnd r ->
        (g, r)
    | GameBegin ->
        g |> doBeginPhase |> doPhase
    | SummonPhase pl ->
        g |> doSummonPhase pl |> doPhase
    | CombatPhase ->
        g |> doCombatPhase |> doPhase
    | AttackPhase order ->
        g |> doAttackPhase order |> doPhase

  let play pl1 pl2 =
    (pl1, pl2)
    ||> Game.init
    |> doPhase
