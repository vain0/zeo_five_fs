namespace ZeoFive.Gui

open System
open System.Drawing
open System.Windows.Forms
open ZeoFive.Core

module Draw =
  let draw (e: PaintEventArgs) (typ, rect: Rectangle) =
    let gfx       = e.Graphics
    let clipRect  = e.ClipRectangle
    in
      match typ with
      | TitleLogo ->
          gfx.DrawString
            ( "ZeoFive"
            , yuGothic 80
            , blackBrush
            , new PointF(float32 rect.Left, float32 rect.Top)
            )
      | Background ->
          gfx.FillRectangle(backgroundBrush, rect)

      | MyButton (text, _) ->
          gfx.FillRectangle
            ( new SolidBrush(Color.DarkGray)
            , new Rectangle(rect.Left + 2, rect.Top + 2, rect.Width, rect.Height)
            )
          gfx.FillRectangle(new SolidBrush(Color.LightGray), rect)
          gfx.DrawString
            ( text
            , yuGothic 11
            , blackBrush
            , RectangleF.ofRectangle rect
            )

      | Card card ->
          let plId = fst card.CardId
          do gfx.FillRectangle
              ( new SolidBrush
                  (match plId with
                    | Player1 -> Color.FromArgb(  0, 200, 255)
                    | Player2 -> Color.FromArgb(200,   0,   0)
                    )
              , rect
              )
          let nameBoxBackBrush =
            new SolidBrush
              (match plId with
                | Player1 -> Color.FromArgb(175, 238, 255)
                | Player2 -> Color.FromArgb(255, 120, 120)
                )
          let nameBoxRect =
            Rectangle
              ( rect.Left + CardFrameWidth
              , rect.Top  + CardFrameWidth
              , cardSize.Width - CardFrameWidth * 2
              , 15
              )
          do gfx.FillRectangle(nameBoxBackBrush, nameBoxRect)

          do gfx.DrawString
              ( card.Spec.Name
              , yuGothic 10
              , blackBrush
              , x = float32 (nameBoxRect.Left)
              , y = float32 (nameBoxRect.Top)
              )
          do
            CardSpec.statusNameList
            |> List.zip (card.Spec |> CardSpec.statusList)
            |> List.iteri (fun i (statusValue, statusName) ->
                let x =
                  rect.Left + CardFrameWidth + 4
                  + (i &&& 1) * 60
                let y =
                  nameBoxRect.Bottom + 3
                  + (i &&& 2) * 7
                do gfx.DrawString
                    ( sprintf "%s%4d" statusName statusValue
                    , yuGothic 10
                    , blackBrush  // TODO: highlight
                    , x = float32 x
                    , y = float32 y
                    )
                )

      | CardBack ->
          gfx.FillRectangle(new SolidBrush(Color.FromArgb(184, 134, 11)), rect)
          gfx.DrawRectangle(blackPen, rect)
          for i in 1..4 do
            let d = 8 * i
            gfx.FillEllipse
              ( new SolidBrush(Color.FromArgb(0, 0, 300 - 50 * i))
              , new Rectangle
                  ( rect.Left + d
                  , rect.Top  + d
                  , cardSize.Width - d * 2
                  , cardSize.Height - d * 2
                  )
              )
