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

        let DragoonSkills =
            [
                Dragoon.HeavyThrust
                Dragoon.BloodForBlood
                Dragoon.ImpulseDrive
                Dragoon.Disembowel
                Dragoon.ChaosThrust
                Dragoon.TrueThrust
                Dragoon.VorpalThrust
                Dragoon.LifeSurge
                Dragoon.FullThrust
            ]
        let DragoonRotation = Calculations.ToRotation DragoonSkills Dragoon.Job

        let BotDSkills =
            [
                Dragoon.BloodOfTheDragon
                Dragoon.Jump
                Dragoon.MirageDive
                Dragoon.SpineshatterDive
                Dragoon.MirageDive
                Dragoon.DoomSpike
                Dragoon.SonicThrust
                Dragoon.DoomSpike
                Dragoon.SonicThrust
                Dragoon.DoomSpike
                Dragoon.SonicThrust
                Dragoon.DoomSpike
                Dragoon.SonicThrust
                Dragoon.DoomSpike
                Dragoon.SonicThrust
                Dragoon.Jump
                Dragoon.MirageDive
                Dragoon.Geirskogul
                Dragoon.Nastrond
            ]
        let BotDRotation = Calculations.ToRotation BotDSkills Dragoon.Job

        let DragoonSetrSkills =
            [
                Dragoon.BattleLitany
                Dragoon.BloodOfTheDragon
                Dragoon.HeavyThrust
                Dragoon.BloodForBlood
                Dragoon.Geirskogul
                Dragoon.ImpulseDrive
                Dragoon.Jump
                Dragoon.MirageDive
                Dragoon.Disembowel
                Dragoon.SpineshatterDive
                Dragoon.MirageDive
                Dragoon.ChaosThrust
                Dragoon.DragonSight
                Dragoon.DragonfireDive
                Dragoon.WheelingThrust
                Dragoon.FangAndClaw
                Dragoon.TrueThrust
                Dragoon.VorpalThrust
                Dragoon.LifeSurge
                Dragoon.FullThrust
                Dragoon.FangAndClaw
                Dragoon.WheelingThrust
                Dragoon.HeavyThrust
                Dragoon.ImpulseDrive
                Dragoon.Disembowel
                Dragoon.ChaosThrust
                Dragoon.Jump
                Dragoon.WheelingThrust
                Dragoon.MirageDive
                Dragoon.FangAndClaw
                Dragoon.Geirskogul
                Dragoon.Nastrond
            ]
        let DragoonSetrRotation = Calculations.ToRotation DragoonSetrSkills Dragoon.Job

        let ShortWheelingFangSkills =
            [
                Dragoon.BloodOfTheDragon
                Dragoon.HeavyThrust
                Dragoon.ImpulseDrive
                Dragoon.Disembowel
                Dragoon.ChaosThrust
                Dragoon.WheelingThrust
                Dragoon.FangAndClaw
                Dragoon.WheelingThrust
                Dragoon.FangAndClaw
            ]
        let ShortWheelingFangRotation = Calculations.ToRotation ShortWheelingFangSkills Dragoon.Job

        [<EntryPoint>]
        let main argv = 
            //printfn "%A" rotation
            match DragoonSetrRotation with
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
                    | Some s -> printf "%d:\tSkill: %20s\tPotency: %d\tCombo: %s\n" i s.Name s.Potency combo
                )
                printfn "MP: %d, TP: %d" job.MP job.TP
            printfn "DPS: %d" (ToDPS DragoonSetrRotation)
            0
