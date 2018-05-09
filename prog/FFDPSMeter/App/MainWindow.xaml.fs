namespace App

open FsXaml

open System
open System.Collections.ObjectModel
open System.ComponentModel
open System.Windows

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow() as this =
    inherit MainWindowBase()

    let shutdown _ =
        MessageBox.Show "Goodbye" |> ignore
        Application.Current.Shutdown ()

    override this.OnLoaded (_, _) =
        //this.ExitButton.Click.Add shutdown
        ()



