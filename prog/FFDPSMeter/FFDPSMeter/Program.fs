namespace FFDPSMeter
    
    module Program =
        open Skills
        open Model
        open Calculations

        let skills = 
            [
                Dragoon.TestDoT
                Dragoon.TestOGCDSkill
                Dragoon.TestOGCDSkill2
                Dragoon.TestSkill
                Dragoon.GCDWithBuff
                Dragoon.TestSkill
                Dragoon.TestOGCDSkill
                Dragoon.TestOGCDSkill
                Dragoon.TestOGCDSkill2
                Dragoon.TestSkill
                Dragoon.TestOGCDSkill2
            ]
            
        let rotation = Calculations.ToRotation skills Dragoon.Job

        [<EntryPoint>]
        let main argv = 
            //printfn "%A" rotation
            match rotation with
            | Rotation (rotation, _) ->
                rotation
                |> List.indexed
                |> List.iter (fun (i, s) ->
                    let skill =
                        match s.ActiveSkill with
                        | None -> "None"
                        | Some s -> s.Name
                    printf "%d: %s\n" i skill
                )
            0
