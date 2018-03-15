namespace FFDPSMeter

    module Model =

        type Job =
            {
                Name        : string
                Speed       : int
                AutoAttack  : int
                MaxMP       : int
                MP          : int
                TP          : int
            }

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
            }

        type Tick =
            {
                ActiveBuffs         : (Buff * int) list
                ActiveDebuffs       : (Buff * int) list
                OGCDTimers          : (Skill * int) list
                GCDtick             : int
                ActiveSkill         : Skill option
                ActiveDoTs          : (Skill * (int * int)) list
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
                        let job =
                            if (lastindex + 1) % 3 = 0 then
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
                                match List.tryFind (fun ((s, _): Skill * (int * int)) -> s.Name = skill.Name) activeDots with
                                | None -> 
                                    (skill, (lastindex + 1, lastindex + 1 + duration)) :: activeDots
                                | Some (s, _) -> 
                                    activeDots
                                    |> List.filter (fun (skill, _) ->
                                        s.Name <> skill.Name
                                    )
                                    |> List.append [(s, (lastindex + 1, lastindex + 1 + duration))]
                            | _ -> activeDots
                        let ogcdTimers =
                            last.OGCDTimers
                            |> List.filter (fun (_, tick) ->
                                tick > lastindex + 1
                            )

                        let rec addDummy (ticklist: Tick list) (n: int) =
                            if n > 0 then
                                let ticklist = ticklist |> List.indexed
                                let lastindex, last = ticklist |> List.last
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
                                let nextTick =
                                    {
                                        ActiveBuffs = activeBuffs
                                        ActiveDebuffs = activeDebuffs
                                        ActiveSkill = None
                                        ActiveDoTs = activeDots
                                        GCDtick = last.GCDtick
                                        OGCDTimers = ogcdTimers
                                    }
                                let ticklist = ticklist |> List.map snd
                                addDummy (List.append ticklist [nextTick]) (n - 1)
                            else
                                ticklist

                        match skill.CooldownType with
                        | GlobalCooldown ->
                            if last.GCDtick < lastindex + 1 then
                                let nextTick =
                                    {
                                        ActiveBuffs = activeBuffs
                                        ActiveDebuffs = activeDebuffs
                                        ActiveSkill = Some skill
                                        ActiveDoTs = activeDots
                                        GCDtick = lastindex + 1 + job.Speed
                                        OGCDTimers = ogcdTimers
                                    }
                                let ticks = ticks |> List.map snd
                                let casttime =
                                    match skill.CastType with
                                    | Instant -> 10
                                    | Time d -> d
                                let ticks = addDummy (List.append ticks [nextTick]) casttime
                                Rotation (ticks, job)
                            else 
                                let dummies = last.GCDtick - lastindex + 1
                                let ticks = ticks |> List.map snd
                                let ticks = addDummy ticks dummies
                                Rotation.add skill (Rotation (ticks, job))
                        | OffGlobalCooldown cd ->
                            match List.tryFind (fun ((s, _): Skill * int) -> s.Name = skill.Name) ogcdTimers with
                            | Some (_, tick) -> 
                                if tick < last.GCDtick then
                                    let ticks = ticks |> List.map snd
                                    let ticks = addDummy ticks (last.GCDtick - tick)
                                    Rotation.add skill (Rotation (ticks, job))
                                else
                                    rotation
                            | None -> 
                                let ogcdTimers = (skill, lastindex + 1 + cd) :: ogcdTimers
                                let nextTick =
                                    {
                                        ActiveBuffs = activeBuffs
                                        ActiveDebuffs = activeDebuffs
                                        ActiveSkill = Some skill
                                        ActiveDoTs = activeDots
                                        GCDtick = last.GCDtick
                                        OGCDTimers = ogcdTimers
                                    }
                                let ticks = ticks |> List.map snd
                                let casttime =
                                    match skill.CastType with
                                    | Instant -> 10
                                    | Time d -> d
                                let ticks = addDummy (List.append ticks [nextTick]) casttime
                                Rotation (ticks, job)
