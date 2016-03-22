namespace ZeoFive.Gui

open System
open System.Drawing

[<AutoOpen>]
module Misc =
  let yuGothic emSizeInt =
    new Font("Yu Gothic", emSizeInt |> float32)

  let blackBrush =
    new SolidBrush(Color.Black)

  let backgroundBrush =
    new SolidBrush(Color.DarkSeaGreen)

  let blackPen =
    new Pen(blackBrush)

module RectangleF =
  let ofRectangle (rect: Rectangle) =
    new RectangleF
      ( float32 rect.Left
      , float32 rect.Top
      , float32 rect.Width
      , float32 rect.Height
      )
