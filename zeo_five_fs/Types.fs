namespace ZeoFive.Core

[<AutoOpen>]
module Types =
  type CardId =
    | CardId of int

  type internal T5<'T> =
    'T * 'T * 'T * 'T * 'T
    
  type PlayerSide =
    | First
    | Second

  type CardSpec =
    {
      Name    : string
      Hp      : int
      Atk     : int
      Itl     : int  // intelligence
      Spd     : int
    }

  type Card =
    {
      Spec    : CardSpec
      CardId  : CardId
      Side    : PlayerSide
      Damage  : int
    }

  type Deck =
    {
      Name    : string
      Cards   : T5<CardSpec>
    }

  type AttackWay =
    | PhysicalAttack
    | MagicalAttack

  type Board =
    Map<CardId, Card>

  type Battlefield =
    Map<PlayerSide, (CardId * option<AttackWay>)>

  type GameResult =
    | Win           of PlayerSide
    | Draw

  type Phase =
    | GameBegin
    | GameEnd       of GameResult
    | SummonPhase   of PlayerSide
    | CombatPhase
    | AttackPhase   of PlayerSide list

  type GameState =
    {
      Board         : Board
      Battlefield   : Battlefield
    }

  type IBrain =
    abstract member Summon: GameState -> CardId
    abstract member Attack: GameState -> AttackWay

  type Player =
    {
      Name    : string
      Deck    : Deck
      Brain   : IBrain
    }

  type Game =
    {
      Players     : Player * Player
      Board       : Board
      Battlefield : Battlefield
      Phase       : Phase
    }
