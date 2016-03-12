namespace ZeoFive.Core

[<AutoOpen>]
module Types =
  type PlayerId =
    | Player1
    | Player2

  type NPCardId =
    | Card1
    | Card2
    | Card3
    | Card4
    | Card5

  type CardId =
    PlayerId * NPCardId

  type AttackWay =
    | PhysicalAttack
    | MagicalAttack

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
      Damage  : int
      PrevWay : option<AttackWay>
    }

  type Deck =
    {
      Name    : string
      Cards   : T5<CardSpec>
    }

  type Hand =
    list<CardId>

  type GameResult =
    | Win           of PlayerId
    | Draw

  type Event =
    | EvSummonSelect  of PlayerId
    | EvSummon        of CardId
    | EvCombat        of Set<PlayerId>
    | EvAttackSelect  of PlayerId
    | EvAttack        of PlayerId * AttackWay
    | EvDamage        of CardId * int
    | EvDie           of CardId
    | EvGameBegin
    | EvGameEnd       of GameResult

  /// 相手プレイヤーからみたプレイヤーの状態
  and PlayerExterior =
    {
      PlayerId    : PlayerId
      Name        : string
      HandCount   : int
      Dohyo       : option<CardId>
    }

  type IBrain =
    abstract member Summon: GameStateFromPlayer -> CardId
    abstract member Attack: GameStateFromPlayer -> AttackWay

  and Player =
    {
      PlayerId    : PlayerId
      Name        : string
      Brain       : IBrain
      Hand        : Hand
      Dohyo       : option<CardId>
    }

  and GameStateFromPlayer =
    {
      Player      : Player
      Opponent    : PlayerExterior
      CardStore   : Map<CardId, Card>
    }

  type Game =
    {
      PlayerStore : Map<PlayerId, Player>
      CardStore   : Map<CardId, Card>
      Kont        : Event list
      ObsSource   : Observable.Source<Game * Event>
    }

  type Entrant =
    {
      Name    : string
      Deck    : Deck
      Brain   : IBrain
    }
