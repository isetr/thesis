namespace FFDPSMeter

    module Calculations =
        open Model

        let ToRotation (skills: Skill list) (job: Job) : Rotation =
            skills
            |>
            List.fold (fun rotation skill ->
                Rotation.add skill rotation
            ) (Rotation.empty job)

        let ToDPS (rotation: Rotation) : int =
            match rotation with
            | Rotation (rotation, job) ->
                let time = (List.length rotation) / 10
                rotation
                |> List.indexed
                |> List.fold (fun dps (index, tick) ->
                    let buffs =
                        tick.ActiveBuffs
                        |> List.fold (fun (s: Skill -> Skill) (v, _) ->
                            match v.Effect with
                            | BuffType.Skill b -> s >> b
                            | _ -> id
                        ) id
                    let debuffs =
                        tick.ActiveDebuffs
                        |> List.fold (fun (s: Skill -> Skill) (v, _) ->
                            match v.Effect with
                            | BuffType.Skill b -> s >> b
                            | _ -> id
                        ) id
                    let combo =
                        match tick.ActiveCombo with
                        | None -> id
                        | Some (c, _) -> 
                            match tick.ActiveSkill with
                            | None -> id
                            | Some s ->
                                if c.Target = s.Name then
                                    c.Effect
                                else
                                    id

                    let autoattack =
                        if index % 30 = 0 then
                            Some <| (buffs >> debuffs) job.AutoAttack
                        else
                            None
                    let activeskill =
                        match tick.ActiveSkill with
                        | None -> None
                        | Some s -> Some <| (combo >> buffs >> debuffs) s

                    let dotPotency =
                        tick.ActiveDoTs
                        |> List.fold (fun s (v, (start, _)) ->
                            if (index - start) % 30 = 0 then
                                s + v.Potency
                            else
                                0
                        ) 0
                    let aaPotency = 
                        match autoattack with
                        | None -> 0
                        | Some autoattack -> autoattack.Potency
                    let activePotency =
                        match activeskill with
                        | None -> 0
                        | Some active -> active.Potency

                    if dotPotency + aaPotency + activePotency > 0 then
                        printfn "DPS (tick %d)\tAutoAttack: %d\tDoT: %d\tSkill: %d\tDamage: %d" index aaPotency dotPotency activePotency dps
                    dps + dotPotency + aaPotency + activePotency
                ) 0
                |> fun x -> x / time
