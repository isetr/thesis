namespace FFDPSMeter

    module Calculations =
        open Model

        let ToRotation (skills: Skill list) (job: Job) : Rotation =
            skills
            |>
            List.fold (fun rotation skill ->
                Rotation.add skill rotation
            ) (Rotation.empty job)

        let ToDamage (rotation: Rotation) (print: bool) : int =
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
                        | Some s -> Some <| (buffs >> debuffs >> combo) s

                    let dotPotency =
                        tick.ActiveDoTs
                        |> List.fold (fun s (v, (start, _)) ->
                            if index > start && (index - start) % 30 = 0 then
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
                    if print then
                        if dotPotency + aaPotency + activePotency > 0 then
                            let buffsPrint =
                                tick.ActiveBuffs
                                |> List.map (fun (b, i) -> sprintf "(%s, %d, %d)" b.Name b.Stacks i)
                            let relativeDPS =
                                if index < 10 then 
                                    0
                                else
                                    (dps / (index / 10))
                            printfn "DPS (tick %d)\tAutoAttack: %d\tDoT: %d\tSkill: %d\tDamage: %d\tDPS: %d" index aaPotency dotPotency activePotency dps relativeDPS
                    dps + dotPotency + aaPotency + activePotency
                ) 0

        let ToDPS (rotation: Rotation) (print: bool) : int =
            let time =
                match rotation with
                | Rotation (rotation, job) ->
                    (List.length rotation) / 10
            ToDamage rotation print
            |> fun x -> 
                if time = 0 then 0 else x / time
