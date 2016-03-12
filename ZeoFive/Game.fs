namespace ZeoFive

open System
open ZeoFive.Core

module Game =
  let doGameEndEvent r (g: Game) =
    cont {
      do g |> Game.happen (EvGameEnd r)
      return! g.EndCont r
    }

  let doSummonEvent cardId g =
    cont {
      let g =
        g
        |> Game.updateDohyo (fst cardId) cardId
        |> Game.updateHand (fst cardId) (List.filter ((<>) cardId))
      do
        g |> Game.happen (EvSummon (cardId))
      return g
    }

  let doSummonSelectEvent plId (g: Game) =
    cont {
      // 全滅判定
      if (g |> Game.player plId).Hand |> List.isEmpty
      then
        return! g |> doGameEndEvent (plId |> Player.inverse |> Win)
      else
        let brain     = (g |> Game.player plId).Brain
        let state     =  g |> Game.state plId
        let cardId    = brain.Summon(state)
        do assert (g |> Game.card cardId |> Card.owner |> (=) plId)
        return! g |> doSummonEvent cardId
    }

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

  let doDieEvent cardId g =
    cont {
      do g |> Game.happen (EvDie cardId)
      let! g =
        g |> doSummonSelectEvent (fst cardId)
      return g
    }

  let doDamageEvent restartCombat (cardId, amount) (g: Game) =
    cont {
      let card    = g |> Game.card cardId
      let card    = { card with Damage = card.Damage + amount }
      let g       = g |> Game.updateCard cardId card
      do g |> Game.happen (EvDamage (cardId, amount))

      // 死亡判定
      if (card |> Card.curHp) <= 0
      then
        let! g = g |> doDieEvent cardId
        return! restartCombat g
      else
        return g
    }

  let doAttackSelectEvent plId g =
    cont {
      let (g, attackWay) =
        g |> attackSelect plId
      let attacker =
        g |> Game.tryDohyoCard plId |> Option.get
      let g =
        g
        |> Game.updateCard
            attacker.CardId
            { attacker with PrevWay = Some attackWay }
      do g |> Game.happen (EvAttackSelect (plId, attackWay))
      return (g, attackWay)
    }

  let doAttackEvent restartCombat (plId, attackWay) g =
    cont {
      let plTarget  = plId |> Player.inverse
      let attacker  = g |> Game.tryDohyoCard plId     |> Option.get
      let target    = g |> Game.tryDohyoCard plTarget |> Option.get
      let amount    =
        attacker
        |> Card.power attackWay
        |> min (target |> Card.curHp)
      let! g =
        g |> doDamageEvent restartCombat (target.CardId, amount)
      return g
    }

  let rec doCombatEvent actedPls g =
    cont {
      do assert (g |> Game.dohyoCards |> Set.count |> (=) 2)
      let! g =
        match g |> nextActor actedPls with
        | None ->
            cont { return g }
        | Some actor ->
            Cont.callCC (fun restartCombat -> cont {
              let! (g, attackWay) =
                g |> doAttackSelectEvent actor
              let! g =
                g |> doAttackEvent restartCombat (actor, attackWay)
              return! g |> doCombatEvent (actedPls |> Set.add actor)
            })
      return! g |> doCombatEvent (Set.empty)
    }

  let startGame g =
    cont {
      let! g = g |> doSummonSelectEvent Player1
      let! g = g |> doSummonSelectEvent Player2
      let! g = g |> doCombatEvent Set.empty
      return Draw  // dummy (combat continues forever)
    }

  let play audience pl1 pl2 =
    Cont.callCC (fun endGame -> cont {
      let g =
        (pl1, pl2)
        ||> Game.init endGame
      let disposables =  // dispose されない
        audience
        |> List.map (fun au -> au g.ObsSource.AsObservable)
      return! g |> startGame
      //do disposables |> List.iter (fun (disp: IDisposable) -> disp.Dispose())
    })
    |> Cont.run
    <| id
