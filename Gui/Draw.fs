namespace ZeoFive.Gui

open System
open System.Drawing
open System.Windows.Forms

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

      | MyButton text ->
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

      | Card cardId ->
          ()

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

