namespace ZeoFive.Gui

open System
open System.Windows.Forms

module Program =
  [<STAThread; EntryPoint>]
  let main argv =
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(false)
    Application.Run(new Main())
    0
