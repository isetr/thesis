open System
open System.Windows

open FsXaml

type App = FsXaml.XAML<"App.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ = 
    let app = App()
    app.Run()
