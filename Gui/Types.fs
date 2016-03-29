namespace ZeoFive.Gui

open System
open System.Drawing
open ZeoFive.Core

[<AutoOpen>]
module Types =
  type Page =
    | TitlePage
      of option<Deck>
    | GamePage
      of Game
    | GameResultPage
      of Game * GameResult

  type GuiObjType =
    | TitleLogo
    | Background
    | MyButton    of (string * (unit -> unit))
    | Card        of Card
    | CardBack

  type GuiObj =
    GuiObjType * Rectangle

  let cardSize = new Size(141, 56)

  [<Literal>]
  let CardFrameWidth = 3
