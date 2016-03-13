namespace ZeoFive.Core

open System

module Game =
  let init endGame ent1 ent2 =
    let deckInit plId (ent: Entrant) =
      T5.zip
        (NPCardId.all |> T5.map (fun c -> (plId, c)))
        (ent.Deck.Cards)
      |> T5.toList
      |> List.map (fun (cardId, spec) ->
          let card = Card.init cardId spec
          in (cardId, card)
          )
    let initCardStore =
      List.append
        (ent1 |> deckInit Player1)
        (ent2 |> deckInit Player2)
      |> Map.ofList
    let initPlayerStore =
      [
        (Player1, Player.init Player1 ent1)
        (Player2, Player.init Player2 ent2)
      ]
      |> Map.ofList
    in
      {
        PlayerStore   = initPlayerStore
        CardStore     = initCardStore
        EndGame       = endGame
        ObsSource     = Observable.Source()
      }

  let happen ev =
    stcont {
      let! g = StateCont.get
      do g.ObsSource.Next(g, ev)
    }

  let getPlayer plId =
    stcont {
      let! g = StateCont.get
      return g.PlayerStore |> Map.find plId
    }

  let getPlayers () =
    stcont {
      let! g = StateCont.get
      return g.PlayerStore |> Map.toList |> List.map snd
    }

  let getCard cardId =
    stcont {
      let! g = StateCont.get
      return g.CardStore |> Map.find cardId
    }

  let tryGetDohyoCard plId =
    stcont {
      let! pl = getPlayer plId
      match pl.Dohyo with
      | None ->
          return None
      | Some cardId ->
          let! card = getCard cardId
          return Some card
    }

  let getDohyoCard plId =
    stcont {
      let! cardOpt = tryGetDohyoCard plId
      return cardOpt |> Option.get
    }

  let getDohyoCards () =
    stcont {
      let! pls      = getPlayers ()
      let dohyo     = pls |> List.choose (fun pl -> pl.Dohyo)
      return dohyo |> Set.ofList
    }
    
  /// プレイヤーにカードが見えているか？
  let isRevealedTo plId cardId =
    stcont {
      let! card         = getCard cardId
      let! dohyoCards   = getDohyoCards ()
      return
        [
          (card |> Card.owner = plId)
          (dohyoCards |> Set.contains cardId)
        ] |> List.exists id
    }

  /// プレイヤー plId からみた場況
  let getState plId =
    stcont {
      let! pl         = getPlayer plId
      let! opponent   = getPlayer (plId |> Player.inverse)
      let! g          = StateCont.get
      let isRevealed _ card =
        g |> StateCont.eval (isRevealedTo plId (card.CardId))
      return
        {
          Player      = pl
          Opponent    = opponent |> Player.exterior
          CardStore   = g.CardStore |> Map.filter isRevealed
        }
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
            (fun cardId ->
                let card = StateCont.eval (getCard cardId) g
                in card.Spec.Spd
                )
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
        ||> init endGame
      let disposables =  // dispose されない
        audience
        |> List.map (fun au -> au g.ObsSource.AsObservable)
      let! (r, g) = StateT.run startGame g
      return r
      //do disposables |> List.iter (fun (disp: IDisposable) -> disp.Dispose())
    })
    |> Cont.run
    <| id
