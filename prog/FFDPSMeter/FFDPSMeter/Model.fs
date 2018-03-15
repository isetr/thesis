namespace FFDPSMeter

    module Model =
        
        type CooldownType =
            | GlobalCooldown
            | OffGlobalCooldown of int

        type CostType =
            | Free
            | MP of int
            | TP of int

        type CastType =
            | Instant
            | Time of int

        type SkillType =
            | Ability
            | Weaponskill

        type ActionType = 
            | Damage of Buff option
            | DamageOverTime of int * Buff option
            | Buff of Buff
            | Debuff of Buff
            | Heal of Buff option
        and BuffType =
            | Job of (Job -> Job)
            | Skill of (Skill -> Skill)
        and Buff =
            {
                Name        : string
                Effect      : BuffType
                Duration    : int
            }
        and Skill =
            {
                Name            : string
                Potency         : int
                Action          : ActionType
                CooldownType    : CooldownType
                CostType        : CostType
                CastType        : CastType
                SkillType       : SkillType
                Combo           : Combo option
            }
        and Combo =
            {
                Name            : string
                Target          : string
                Effect          : Skill -> Skill
                DistruptedByGCD : bool
            }
        and Job =
            {
                Name        : string
                Speed       : int
                AutoAttack  : Skill
                MaxMP       : int
                MP          : int
                TP          : int
            }

        type Tick =
            {
                ActiveBuffs         : (Buff * int) list
                ActiveDebuffs       : (Buff * int) list
                OGCDTimers          : (Skill * int) list
                GCDtick             : int
                ActiveSkill         : Skill option
                ActiveDoTs          : (Skill * (int * int)) list
                ActiveCombo         : (Combo * int) option
            }

        type Rotation = 
            | Rotation of (Tick list) * Job

            static member empty (job: Job) =
                ([{
                    ActiveBuffs = []
                    ActiveDebuffs = []
                    ActiveSkill = None
                    ActiveDoTs = []
                    GCDtick = 0
                    OGCDTimers = []
                    ActiveCombo = None
                }], job)
                |> Rotation

            static member add (skill: Skill) (rotation: Rotation) =
                match rotation with
                | Rotation (ticks, job) ->
                    match ticks with
                    | [] -> failwith "How o.o"
                    | ticks ->
                        let ticks = ticks |> List.indexed
                        let lastindex, last = ticks |> List.last
                        let updateJob (job: Job) (lastindex: int) (last: Tick) =
                            if (lastindex + 1) % 30 = 0 then
                                let newMP = 
                                    let mp = int (float job.MP * 1.02)
                                    if mp > job.MaxMP then
                                        job.MaxMP
                                    else
                                        mp
                                let newTP =
                                    if job.TP + 30 > 1000 then
                                        1000
                                    else
                                        job.TP + 30
                                let buffs =
                                    last.ActiveBuffs
                                    |> List.fold (fun (s: Job -> Job) (v, _) ->
                                        match v.Effect with
                                        | Skill _ -> id
                                        | Job v -> s >> v
                                    ) id
                                buffs {job with MP = newMP; TP = newTP}
                            else
                                job
                        let job = updateJob job lastindex last
                        let addToBuffList (b: Buff) (list: (Buff * int) list)=
                            match List.tryFind (fun ((buff, _): Buff * int) -> b.Name = buff.Name) list with
                            | None -> 
                                (b, lastindex + 1 + b.Duration) :: list
                            | Some (b, _) -> 
                                list
                                |> List.filter (fun (buff, _) ->
                                    b.Name <> buff.Name
                                )
                                |> List.append [(b, lastindex + 1 + b.Duration)]
                        let activeBuffs =
                            let activeBuffs =
                                last.ActiveBuffs
                                |> List.filter (fun (_, tick) ->
                                    tick > lastindex + 1
                                )
                            match skill.Action with
                            | ActionType.DamageOverTime (_, b)
                            | ActionType.Heal b 
                            | ActionType.Damage b ->
                                match b with
                                | None -> activeBuffs
                                | Some b -> addToBuffList b activeBuffs
                            | ActionType.Buff b -> addToBuffList b activeBuffs
                            | _ -> activeBuffs
                        let activeDebuffs =
                            let activeDebuffs =
                                last.ActiveDebuffs
                                |> List.filter (fun (_, tick) ->
                                    tick > lastindex + 1
                                )
                            match skill.Action with
                            | ActionType.Debuff b -> addToBuffList b activeDebuffs
                            | _ -> activeDebuffs
                        let activeDots =
                            let activeDots =
                                last.ActiveDoTs
                                |> List.filter (fun (_, (_, tick)) ->
                                    tick > lastindex + 1
                                )
                            match skill.Action with
                            | ActionType.DamageOverTime (duration, _) ->
                                let buffs =
                                    last.ActiveBuffs
                                    |> List.fold (fun (s: Skill -> Skill) (v, _) ->
                                        match v.Effect with
                                        | BuffType.Skill b -> s >> b
                                        | _ -> id
                                    ) id
                                let debuffs =
                                    last.ActiveDebuffs
                                    |> List.fold (fun (s: Skill -> Skill) (v, _) ->
                                        match v.Effect with
                                        | BuffType.Skill b -> s >> b
                                        | _ -> id
                                    ) id
                                let combo =
                                    match last.ActiveCombo with
                                    | None -> id
                                    | Some (c, _) -> 
                                        match last.ActiveSkill with
                                        | None -> id
                                        | Some s ->
                                            if c.Target = s.Name then
                                                c.Effect
                                            else
                                                id
                                match List.tryFind (fun ((s, _): Skill * (int * int)) -> s.Name = skill.Name) activeDots with
                                | None -> 
                                    ((combo >> buffs >> debuffs) skill, (lastindex + 1, lastindex + 1 + duration)) :: activeDots
                                | Some (s, _) -> 
                                    activeDots
                                    |> List.filter (fun (skill, _) ->
                                        s.Name <> skill.Name
                                    )
                                    |> List.append [((combo >> buffs >> debuffs) skill, (lastindex + 1, lastindex + 1 + duration))]
                            | _ -> activeDots
                        let ogcdTimers =
                            last.OGCDTimers
                            |> List.filter (fun (_, tick) ->
                                tick > lastindex + 1
                            )
                        let activeCombo = 
                            match skill.Combo with
                            | Some c -> Some (c, lastindex + 1 + 50)
                            | None -> 
                                match last.ActiveCombo with
                                | None -> None
                                | Some (c, i) as combo ->
                                    if i < lastindex + 1 then
                                        None
                                    else
                                        match skill.CooldownType with
                                        | OffGlobalCooldown _ -> combo
                                        | GlobalCooldown ->
                                            if skill.Name = c.Target then
                                                Some (c, 0)
                                            else
                                                if c.DistruptedByGCD then
                                                    None
                                                else
                                                    combo

                        let rec addDummy (ticklist: Tick list) (n: int) (job: Job) =
                            if n > 0 then
                                let ticklist = ticklist |> List.indexed
                                let lastindex, last = ticklist |> List.last
                                let job = updateJob job lastindex last
                                let activeBuffs =
                                    last.ActiveBuffs
                                    |> List.filter (fun (_, tick) ->
                                        tick > lastindex + 1
                                    )
                                let activeDebuffs =
                                    last.ActiveDebuffs
                                    |> List.filter (fun (_, tick) ->
                                        tick > lastindex + 1
                                    )
                                let activeDots =
                                    last.ActiveDoTs
                                    |> List.filter (fun (_, (_, tick)) ->
                                        tick > lastindex + 1
                                    )
                                let ogcdTimers =
                                    last.OGCDTimers
                                    |> List.filter (fun (_, tick) ->
                                        tick > lastindex + 1
                                    )
                                let activeCombo = 
                                    match last.ActiveCombo with
                                    | None -> None
                                    | Some (c, i) as combo ->
                                        if i < lastindex + 1 then
                                            None
                                        else
                                            combo
                                let nextTick =
                                    {
                                        ActiveBuffs = activeBuffs
                                        ActiveDebuffs = activeDebuffs
                                        ActiveSkill = None
                                        ActiveDoTs = activeDots
                                        GCDtick = last.GCDtick
                                        OGCDTimers = ogcdTimers
                                        ActiveCombo = activeCombo
                                    }
                                let ticklist = ticklist |> List.map snd
                                addDummy (List.append ticklist [nextTick]) (n - 1) job
                            else
                                job, ticklist

                        match skill.CooldownType with
                        | GlobalCooldown ->
                            if last.GCDtick < lastindex + 1 then
                                let job =
                                    match skill.CostType with
                                    | Free -> Some job
                                    | MP d ->
                                        if job.MP - d < 0 then
                                            None
                                        else
                                            Some {job with MP = job.MP - d}
                                    | TP d ->
                                        if job.TP - d < 0 then
                                            None
                                        else
                                            Some {job with TP = job.TP - d}
                                match job with
                                | None -> rotation
                                | Some job ->
                                    let nextTick =
                                        {
                                            ActiveBuffs = activeBuffs
                                            ActiveDebuffs = activeDebuffs
                                            ActiveSkill = Some skill
                                            ActiveDoTs = activeDots
                                            GCDtick = lastindex + 1 + job.Speed
                                            OGCDTimers = ogcdTimers
                                            ActiveCombo = activeCombo
                                        }
                                    let ticks = ticks |> List.map snd
                                    let casttime =
                                        match skill.CastType with
                                        | Instant -> 10
                                        | Time d -> d
                                    let job, ticks = addDummy (List.append ticks [nextTick]) casttime job
                                    Rotation (ticks, job)
                            else 
                                let dummies = last.GCDtick - lastindex + 1
                                let ticks = ticks |> List.map snd
                                let job, ticks = addDummy ticks dummies job
                                Rotation.add skill (Rotation (ticks, job))
                        | OffGlobalCooldown cd ->
                            match List.tryFind (fun ((s, _): Skill * int) -> s.Name = skill.Name) ogcdTimers with
                            | Some (_, tick) -> 
                                if tick < last.GCDtick then
                                    let ticks = ticks |> List.map snd
                                    let job, ticks = addDummy ticks (last.GCDtick - tick) job
                                    Rotation.add skill (Rotation (ticks, job))
                                else
                                    rotation
                            | None -> 
                                let job =
                                    match skill.CostType with
                                    | Free -> Some job
                                    | MP d ->
                                        if job.MP - d < 0 then
                                            None
                                        else
                                            Some {job with MP = job.MP - d}
                                    | TP d ->
                                        if job.TP - d < 0 then
                                            None
                                        else
                                            Some {job with TP = job.TP - d}
                                match job with
                                | None -> rotation
                                | Some job ->
                                    let ogcdTimers = (skill, lastindex + 1 + cd) :: ogcdTimers
                                    let nextTick =
                                        {
                                            ActiveBuffs = activeBuffs
                                            ActiveDebuffs = activeDebuffs
                                            ActiveSkill = Some skill
                                            ActiveDoTs = activeDots
                                            GCDtick = last.GCDtick
                                            OGCDTimers = ogcdTimers
                                            ActiveCombo = activeCombo
                                        }
                                    let ticks = ticks |> List.map snd
                                    let casttime =
                                        match skill.CastType with
                                        | Instant -> 10
                                        | Time d -> d
                                    let job, ticks = addDummy (List.append ticks [nextTick]) casttime job
                                    Rotation (ticks, job)
