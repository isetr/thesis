namespace App

open System

type SkillView (s: FFDPSMeter.Model.Skill) =
    member this.Name = s.Name
    member this.Potency = s.Potency
    member this.ID = s.ID
    member this.Image = "/Resources/Skills/" + (s.Name.Replace(" ", "")) + ".png"

    member this.ToMLModel (s: FFDPSMeter.Model.Skill list) =
        s
        |> List.find (fun skill -> skill.ID = this.ID)
