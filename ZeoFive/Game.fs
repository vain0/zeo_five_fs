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

  let player plId g =
    g.PlayerStore |> Map.find plId

  type UpdateAtom =
    | UpdateEnd       of GameResult
    | UpdatePlayer    of Player
    | UpdateDohyo     of CardId
    | UpdateCard      of Card
    | UpdateHand      of PlayerId * (Hand -> Hand)

  type Update =
    | Update of list<UpdateAtom>
  with
    static member Unit =
      Update []

    static member Combine(Update l, Update r) =
      Update (List.append l r)

    static member Apply(g, Update us) =
      let rec applyAtom g =
        function
        | UpdateEnd _ -> g

        | UpdatePlayer pl ->
            let playerStore =
              g.PlayerStore |> Map.add (pl.PlayerId) pl
            in
              { g with PlayerStore = playerStore }

        | UpdateDohyo cardId ->
            let plId  = fst cardId
            let pl    = g |> player plId
            let pl    = { pl with Dohyo = Some cardId }
            in
              UpdatePlayer pl |> applyAtom g

        | UpdateCard card ->
            let cardStore =
              g.CardStore |> Map.add (card.CardId) card
            in
              { g with CardStore = cardStore }

        | UpdateHand (plId, f) ->
            let pl  = g |> player plId
            let pl  = { pl with Hand = (pl.Hand |> f) }
            in
              UpdatePlayer pl |> applyAtom g
      in
        us |> List.fold applyAtom g

  type GameMonad<'T, 'R> =
    UpdateT<Game, Cont<'R, Update * 'T>>

  let update ua =
    upcont {
      do! UpdateT.update (Update [ua])
    }

  let happen ev: GameMonad<_, _> =
    upcont {
      let! g = UpdateT.get ()
      do g.ObsSource.Next(g, ev)
    }

  let getPlayer plId =
    upcont {
      let! g = UpdateT.get ()
      return g |> player plId
    }

  let getPlayers () =
    upcont {
      let! g = UpdateT.get ()
      return g.PlayerStore |> Map.toList |> List.map snd
    }

  let getCard cardId =
    upcont {
      let! g = UpdateT.get ()
      return g.CardStore |> Map.find cardId
    }

  let tryGetDohyoCard plId =
    upcont {
      let! pl = getPlayer plId
      match pl.Dohyo with
      | None ->
          return None
      | Some cardId ->
          let! card = getCard cardId
          return Some card
    }

  let getDohyoCard plId =
    upcont {
      let! cardOpt = tryGetDohyoCard plId
      return cardOpt |> Option.get
    }

  let getDohyoCards () =
    upcont {
      let! pls      = getPlayers ()
      let dohyo     = pls |> List.choose (fun pl -> pl.Dohyo)
      return dohyo |> Set.ofList
    }
    
  /// プレイヤーにカードが見えているか？
  let isRevealedTo plId cardId =
    upcont {
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
    upcont {
      let! pl         = getPlayer plId
      let! opponent   = getPlayer (plId |> Player.inverse)
      let! g          = UpdateT.get ()
      let isRevealed _ card =
        g |> UpdateCont.eval (isRevealedTo plId (card.CardId))
      return
        {
          Player      = pl
          Opponent    = opponent |> Player.exterior
          CardStore   = g.CardStore |> Map.filter isRevealed
        }
    }

  let doGameEndEvent r =
    upcont {
      do! happen (EvGameEnd r)
      let! g = UpdateT.get ()
      return! UpdateCont.liftCont (g.EndGame r)
    }

  let doSummonEvent cardId =
    upcont {
      do! update (UpdateDohyo cardId)
      do! update (UpdateHand ((fst cardId), (List.filter ((<>) cardId))))
      do! happen (EvSummon (cardId))
    }

  let doSummonSelectEvent plId =
    upcont {
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
    upcont {
      let! dohyoCards = getDohyoCards ()
      let! g = UpdateT.get ()
      return
        dohyoCards
        |> Set.toList
        |> List.filter (fun (plId, _) ->
            actedPls |> Set.contains plId |> not
            )
        |> List.tryMaxBy
            (fun cardId ->
                let card = UpdateCont.eval (getCard cardId) g
                in card.Spec.Spd
                )
        |> Option.map fst
    }

  let attackSelect plId =
    upcont {
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
    upcont {
      do! happen (EvDie cardId)
      return! doSummonSelectEvent (fst cardId)
    }

  let doDamageEvent restartCombat (cardId, amount) =
    upcont {
      let! card   = getCard cardId
      let card    = { card with Damage = card.Damage + amount }
      do! update (UpdateCard card)
      do! happen (EvDamage (cardId, amount))

      // 死亡判定
      if (card |> Card.curHp) <= 0 then
        return! doDieEvent cardId
    }

  let doAttackSelectEvent plId =
    upcont {
      let! attackWay  = attackSelect plId
      let! attacker   = getDohyoCard plId
      let  attacker   = { attacker with PrevWay = Some attackWay }
      do! update (UpdateCard attacker)
      do! happen (EvAttackSelect (plId, attackWay))
      return attackWay
    }

  let doAttackEvent restartCombat (plId, attackWay) =
    upcont {
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
    upcont {
      let! dohyoCards = getDohyoCards ()
      do assert (dohyoCards |> Set.count |> (=) 2)
      let! actorOpt = nextActor actedPls
      do!
        match actorOpt with
        | None ->
            upcont { return () }
        | Some actor ->
            UpdateCont.callCC (fun restartCombat -> upcont {
              let! attackWay = doAttackSelectEvent actor
              do! doAttackEvent restartCombat (actor, attackWay)
              do! happen (EvCombat actedPls)
              return! doCombatEvent (actedPls |> Set.add actor)
              })
      // repeat forever (``Game.EndGame`` is called to end game)
      return! doCombatEvent (Set.empty)
    }

  let startGame =
    upcont {
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
      return UpdateCont.eval startGame g
      //do disposables |> List.iter (fun (disp: IDisposable) -> disp.Dispose())
    })
    |> Cont.run
    <| id
