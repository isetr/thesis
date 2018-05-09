namespace FFDPSMeter
    
    module Program =
        open Skills
        open Model
        open Calculations
        open MCModel2

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
                Dragoon.Invigorate
                Dragoon.WheelingThrust
                Dragoon.MirageDive
                Dragoon.FangAndClaw
                Dragoon.Geirskogul
                Dragoon.Nastrond
                //Dragoon.Invigorate
            ]
        let DragoonSetrRotation = Calculations.ToRotation DragoonSetrSkills Dragoon.Job
        
        [<EntryPoint>]
        let main argv = 
            //match DragoonSetrRotation with
            //| Rotation (rotation, job) ->
            //    rotation
            //    |> List.indexed
            //    |> List.iter (fun (i, tick) ->
            //        let combo =
            //            match tick.ActiveCombo with
            //            | None -> "None"
            //            | Some (c, _) -> c.Target
            //        match tick.ActiveSkill with
            //        | None -> ()
            //        | Some s -> printf "%d:\tSkill: %20s\tPotency: %d\tCombo: %s\n" i s.Name s.Potency combo
            //    )
            //    printfn "MP: %d, TP: %d" job.MP job.TP
            //printfn "DPS: %d" (ToDPS DragoonSetrRotation true)


            let job = Dragoon.Job
            let skillset = 
                [
                    Dragoon.BattleLitany
                    Dragoon.BloodForBlood
                    Dragoon.BloodOfTheDragon
                    Dragoon.ChaosThrust
                    Dragoon.Disembowel
                    Dragoon.DoomSpike
                    Dragoon.DragonfireDive
                    Dragoon.DragonSight
                    Dragoon.FangAndClaw
                    Dragoon.FullThrust
                    Dragoon.Geirskogul
                    Dragoon.HeavyThrust
                    Dragoon.ImpulseDrive
                    Dragoon.Invigorate
                    Dragoon.Jump
                    Dragoon.LifeSurge
                    Dragoon.MirageDive
                    Dragoon.Nastrond
                    Dragoon.PiercingTalon
                    Dragoon.SonicThrust
                    Dragoon.SpineshatterDive
                    Dragoon.TrueThrust
                    Dragoon.VorpalThrust
                    Dragoon.WheelingThrust
                ]
            let job = Paladin.Job
            let skillset = Paladin.Skillset
                
            printf "\nDuration: "
            let duration = int <| System.Console.ReadLine ()
            printf "\nThreads: "
            let threads = int <| System.Console.ReadLine ()
            printf "\nEpisodes: "
            let episodes = int <| System.Console.ReadLine ()
            printf "\nGenerations: "
            let generations = int <| System.Console.ReadLine ()
            printf "\nSample: "
            let sample = int <| System.Console.ReadLine ()
            //let Q, _ = mcControlImportanceSampling (job, skillset) 1800 (fromRotation (Rotation.empty job)) 30 (createRandomPolicy (skillset.Length)) 0.9 None
            let folder = sprintf "%s-%d-%d-%d" (System.DateTime.Now.ToString("yyyyMMdd-Hmmss")) generations episodes duration

            let geneticPolicy (generations: int) (startingPolicy: State -> float list) (dur: int) (episodes: int) (threads: int) =
                let T = System.DateTime.Now
                List.init generations id
                |> List.fold (fun (Q, C, s) i ->
                    let startT = System.DateTime.Now
                    let epsilon = 1. / (5. * System.Math.Log (float i + 5.))
                    if i % 1 = 0 then printfn "\nGeneration #%d\t | Epsilon:\t%f" i (if i = 0 then 0. else epsilon)
                    let Q, C, _ = mcControlImportanceSampling (job, skillset) dur episodes s Q C 0.7 None threads
                    let policy = (createEpsilonGreedyPolicy Q (skillset.Length) epsilon)
                    let endT = System.DateTime.Now
                    let QCount =
                        Q 
                        |> Map.fold (fun s k v ->
                            s + Map.count v
                        ) 0
                    printfn "\nGeneration time: %A\tTotal elapsed time: %A\tQ size: %d" (endT - startT) (endT - T) QCount //(Map.count Q)
                    if sample > 0 && i % sample = 0 then
                        runPolicy (job, skillset) policy dur Q None folder
                    Q, C, policy
                ) (Map.empty, Map.empty, startingPolicy)
            //printfn "%A" Q
            //printfn "\n Simulation by Q policy: " 
            let Q, C, policy = geneticPolicy generations (createRandomPolicy (skillset.Length)) duration episodes threads
            let policy = createEpsilonGreedyPolicy Q (skillset.Length) 0.05
            runPolicy (job, skillset) policy duration Q None folder
            System.Console.ReadKey () |> ignore
            0
