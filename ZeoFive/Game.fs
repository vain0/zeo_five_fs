namespace ZeoFive

open System
open ZeoFive.Core

module Game =
  let happen ev =
    stcont {
      let! g = StateCont.get
      do g.ObsSource.Next(g, ev)
    }

  let getPlayer plId =
    stcont {
      let! g = StateCont.get
      return g |> Game.player plId
    }

  let getCard cardId =
    stcont {
      let! g = StateCont.get
      return g |> Game.card cardId
    }

  let getDohyoCard plId =
    stcont {
      let! g = StateCont.get
      return g |> Game.tryDohyoCard plId |> Option.get
    }

  let getDohyoCards () =
    stcont {
      let! g = StateCont.get
      return g |> Game.dohyoCards
    }

  let getState plId =
    stcont {
      let! g = StateCont.get
      return g |> Game.state plId
    }

  let updatePlayer pl =
    stcont {
      let! g = StateCont.get
      let playerStore =
        g.PlayerStore |> Map.add (pl.PlayerId) pl
      let g = { g with PlayerStore = playerStore }
      return! StateCont.put g
    }

  let updateDohyo cardId =
    stcont {
      let plId = fst cardId
      let! g = StateCont.get
      let! pl = getPlayer plId
      let  pl = { pl with Dohyo = Some cardId }
      return! updatePlayer pl
    }

  let updateHand plId f =
    stcont {
      let! pl = getPlayer plId
      let pl = { pl with Hand = (pl.Hand |> f) }
      return! updatePlayer pl
    }

  let updateCard card =
    stcont {
      let! g = StateCont.get
      let cardStore =
        g.CardStore |> Map.add (card.CardId) card
      let g = { g with CardStore = cardStore }
      return! StateCont.put g
    }

  let doGameEndEvent r =
    stcont {
      do! happen (EvGameEnd r)
      let! g = StateCont.get
      return! StateCont.liftCont (g.EndGame r)
    }

  let doSummonEvent cardId =
    stcont {
      do! updateDohyo cardId
      do! updateHand (fst cardId) (List.filter ((<>) cardId))
      do! happen (EvSummon (cardId))
    }

  let doSummonSelectEvent plId =
    stcont {
      let! pl = getPlayer plId
      // 全滅判定
      if pl.Hand |> List.isEmpty then
        return! doGameEndEvent (plId |> Player.inverse |> Win)
      else
        let brain     = pl.Brain
        let! state    = getState plId
        let cardId    = brain.Summon(state)
        let! card     = getCard cardId
        do assert (card |> Card.owner |> (=) plId)
        do! happen (EvSummonSelect cardId)
        return! doSummonEvent cardId
    }

  let nextActor actedPls =
    stcont {
      let! dohyoCards = getDohyoCards ()
      let! g = StateCont.get
      return
        dohyoCards
        |> Set.toList
        |> List.filter (fun (plId, _) ->
            actedPls |> Set.contains plId |> not
            )
        |> List.tryMaxBy
            (fun cardId -> (g |> Game.card cardId).Spec.Spd)
        |> Option.map fst
    }

  let attackSelect plId =
    stcont {
      let! attacker = getDohyoCard plId
      let! pl       = getPlayer plId
      match attacker.PrevWay with
      | Some prev ->
          return prev |> AttackWay.inverse
      | None ->
          let brain   = pl.Brain
          let! state  = getState plId
          return brain.Attack(state)
    }

  let doDieEvent cardId =
    stcont {
      do! happen (EvDie cardId)
      return! doSummonSelectEvent (fst cardId)
    }

  let doDamageEvent restartCombat (cardId, amount) =
    stcont {
      let! card   = getCard cardId
      let card    = { card with Damage = card.Damage + amount }
      do! updateCard card
      do! happen (EvDamage (cardId, amount))

      // 死亡判定
      if (card |> Card.curHp) <= 0 then
        return! doDieEvent cardId
    }

  let doAttackSelectEvent plId =
    stcont {
      let! attackWay  = attackSelect plId
      let! attacker   = getDohyoCard plId
      do! updateCard { attacker with PrevWay = Some attackWay }
      do! happen (EvAttackSelect (plId, attackWay))
      return attackWay
    }

  let doAttackEvent restartCombat (plId, attackWay) =
    stcont {
      let plTarget  = plId |> Player.inverse
      let! attacker = getDohyoCard plId
      let! target   = getDohyoCard plTarget
      let amount    =
        attacker
        |> Card.power attackWay
        |> min (target |> Card.curHp)
      do! happen (EvAttack plId)
      return! doDamageEvent restartCombat (target.CardId, amount)
    }

  let rec doCombatEvent actedPls =
    stcont {
      let! dohyoCards = getDohyoCards ()
      do assert (dohyoCards |> Set.count |> (=) 2)
      let! actorOpt = nextActor actedPls
      do!
        match actorOpt with
        | None ->
            stcont { return () }
        | Some actor ->
            StateCont.callCC (fun restartCombat -> stcont {
              let! attackWay = doAttackSelectEvent actor
              do! doAttackEvent restartCombat (actor, attackWay)
              do! happen (EvCombat actedPls)
              return! doCombatEvent (actedPls |> Set.add actor)
              })
      // repeat forever (``Game.EndGame`` is called to end game)
      return! doCombatEvent (Set.empty)
    }

  let startGame =
    stcont {
      do! happen (EvGameBegin)
      do! doSummonSelectEvent Player1
      do! doSummonSelectEvent Player2
      do! doCombatEvent Set.empty
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
      let! (r, g) = StateT.run startGame g
      return r
      //do disposables |> List.iter (fun (disp: IDisposable) -> disp.Dispose())
    })
    |> Cont.run
    <| id
