namespace App

open System

type SkillView (s: FFDPSMeter.Model.Skill, place: int) =
    member this.Name = s.Name
    member this.Potency = s.Potency
    member this.ID = s.ID
    member this.Image = "/Resources/Skills/" + (s.Name.Replace(" ", "")) + ".png"
    member this.Place = place

    member this.ToMLModel (s: FFDPSMeter.Model.Skill list) =
        s
        |> List.find (fun skill -> skill.ID = this.ID)

type ChartEntry<'A, 'B> (key: 'A, value: 'B) =
    member this.Key = key
    member this.Value = value

type RelayObserver<'T> (onNext) =
    let onNext = onNext
    interface IObserver<'T> with
        member __.OnNext value = onNext value
        member __.OnError error = ()
        member __.OnCompleted () = ()
