namespace ZeoFive.Gui

open System
open System.Drawing
open System.Windows.Forms
open ZeoFive.Core

type Main () =
  inherit Form
    ( ClientSize    = Size(640, 480)
    , Text          = "ZeoFive Player"
    )

  let winSize = base.ClientSize

  let logBoxSize = new Size(winSize.Width - 180, 70)

  let logBox =
    new TextBox
      ( Size        = logBoxSize
      , Location    = new Point(0, winSize.Height - logBoxSize.Height)
      , Multiline   = true
      , ScrollBars  = ScrollBars.Vertical
      //, Enabled     = false
      , Font        = yuGothic 11
      )
  do
    base.Controls.Add(logBox)

  let components =
    let bg = 
      (Background, new Rectangle(0, 0, winSize.Width, winSize.Height))
    in
      function
      | TitlePage openedDeck ->
          [
            yield bg
            yield (TitleLogo, new Rectangle(190, 40, winSize.Width, winSize.Height))

            // hand
            let rects =
              [0..4]
              |> List.map (fun i ->
                  let x = 20
                  let y = 20 + i * (cardSize.Height + 13)
                  in new Rectangle(x, y, cardSize.Width, cardSize.Height)
                  )

            match openedDeck with
            | Some deck ->
                let cards =
                  T5.zip deck.Cards NPCardId.all
                  |> T5.toList
                  |> List.zip rects
                for (rect, (spec, npcardId)) in cards do
                  yield (Card.init (Player1, npcardId) spec |> Card, rect)
            | None ->
                for rect in rects do
                  yield (CardBack, rect)

            // buttons
            yield!
              ["Open Deck"; "Battle vs. CPU"; "Rule"]
              |> List.mapi (fun i text ->
                  ( MyButton text
                  , new Rectangle
                      ( 200 + i * (120 + 10), 170
                      , 120, 30
                      )
                  ))
          ]

      | GamePage g -> []
      | GameResultPage (g, r) -> []

  let mutable curPage =
    TitlePage None

  let mutable curComponents =
    curPage |> components

  override this.OnPaint(e) =
    curComponents |> List.iter (Draw.draw e)
