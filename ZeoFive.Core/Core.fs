namespace ZeoFive.Core

open Chessie.ErrorHandling

module NPCardId =
  let all = (Card1, Card2, Card3, Card4, Card5)

  let indexed =
    T5.zip (0, 1, 2, 3, 4) all |> T5.toList

module CardSpec =
  let statusNameList =
    ["HP"; "SPD"; "ATK"; "ITL"]

  let statusList (self: CardSpec) =
    [self.Hp; self.Spd; self.Atk; self.Itl]

  let statusTotal (self: CardSpec) =
    self |> statusList |> List.sum

module Card =
  let init cardId spec =
    {
      Spec    = spec
      CardId  = cardId
      Damage  = 0
      PrevWay = None
    }

  let curHp (card: Card) =
    card.Spec.Hp - card.Damage

  let isAlive card =
    curHp card > 0

  let isDead card =
    card |> isAlive |> not

  let power attackWay (card: Card) =
    match attackWay with
    | PhysicalAttack -> card.Spec.Atk
    | MagicalAttack  -> card.Spec.Itl

  let owner (card: Card) =
    card.CardId |> fst

module Deck =
  let toJson self =
    Serialize.serializeJson<Deck>(self)

  let ofJson json =
    Serialize.deserializeJson<Deck>(json) |> UnvalidatedDeck

  let validate (UnvalidatedDeck self) =
    let validateStatusTotal card =
      if card |> CardSpec.statusTotal = 200
      then card |> pass
      else card |> warn (InvalidStatusTotal card) 

    let validateEachStatus card =
      [
        let statusMap =
          List.zip
            CardSpec.statusNameList
            (card |> CardSpec.statusList)
        for (statusName, status) in statusMap do
          if status < 0 then
            yield card |> warn (InvalidStatusValue (card, statusName, status))

        if card.Hp = 0 then
          yield card |> warn (InvalidStatusValue (card, "HP", 0))
        ]
      |> List.toSeq
      |> Trial.collect

    self.Cards
    |> T5.toList
    |> List.map (validateStatusTotal >> Trial.bind validateEachStatus)
    |> List.toSeq
    |> Trial.collect
    |> (function
        | Pass _        -> self |> ok
        | Warn (_, msg)
        | Fail (msg)    -> msg |> Bad
        )

module Player =
  let allIds =
    [Player1; Player2]

  let inverse =
    function
    | Player1 -> Player2
    | Player2 -> Player1

  let init plId (entrant: Entrant): Player =
    {
      PlayerId      = plId
      Name          = entrant.Name
      Brain         = entrant.Brain
      Hand =
        NPCardId.all
        |> T5.map (fun cId -> (plId, cId))
        |> T5.toList
      Dohyo         = None
    }

  let exterior (self: Player) =
    {
      PlayerId      = self.PlayerId
      Name          = self.Name
      HandCount     = self.Hand |> List.length
      Dohyo         = self.Dohyo
    }

module AttackWay =
  let inverse =
    function
    | PhysicalAttack -> MagicalAttack
    | MagicalAttack  -> PhysicalAttack

module Brain =
  type StupidBrain() =
    interface IBrain with
      member this.Summon(state: GameStateFromPlayer) =
        state.Player.Hand
        |> List.head

      member this.Attack(state: GameStateFromPlayer) =
        PhysicalAttack

module Error =
  let toString =
    function
    | InvalidStatusTotal spec ->
        sprintf "Card status total should be 200 but is %d"
          (spec |> CardSpec.statusTotal)
    | InvalidStatusValue (card, statusName, status) ->
        sprintf "%s value %d is invalid." statusName status
