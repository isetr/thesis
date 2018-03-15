namespace FFDPSMeter
    
    module Program =
        open Skills
        open Model
        open Calculations

        let skills = 
            [
                TestJob.TestDoT
                TestJob.TestOGCDSkill
                TestJob.TestOGCDSkill2
                TestJob.TestSkill
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.TestSkill
                TestJob.TestOGCDSkill
                TestJob.TestOGCDSkill
                TestJob.TestOGCDSkill2
                TestJob.TestSkill
                TestJob.TestOGCDSkill2
            ]

        let skillsTPdeplate =
            [
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.GCDWithBuff
                TestJob.TestSkill
                TestJob.TestSkill
                TestJob.TestSkill
            ]

        let comboSkills =
            [
                TestJob.GCDWithBuff
                TestJob.TestDoT
                TestJob.Combo1
                TestJob.Combo2
                TestJob.Combo3
                TestJob.Combo1
                TestJob.Combo2
                TestJob.Combo3
                TestJob.Combo1
                TestJob.Combo2
                TestJob.TestOGCDSkill
                TestJob.Combo3
            ]
            
        let rotation = Calculations.ToRotation comboSkills TestJob.Job

        [<EntryPoint>]
        let main argv = 
            //printfn "%A" rotation
            match rotation with
            | Rotation (rotation, job) ->
                rotation
                |> List.indexed
                |> List.iter (fun (i, s) ->
                    let combo =
                        match s.ActiveCombo with
                        | None -> "None"
                        | Some (c, _) -> sprintf "%s to %s" c.Name c.Target
                    match s.ActiveSkill with
                    | None -> ()
                    | Some s -> printf "%d:\tSkill: %s,\tPotency: %d\tCombo: %s\n" i s.Name s.Potency combo
                )
                printfn "MP: %d, TP: %d" job.MP job.TP
            printfn "DPS: %d" (ToDPS rotation)
            0
