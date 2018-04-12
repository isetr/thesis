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
            | Damage of Buff list option
            | DamageOverTime of potency:int * duration:int * Buff option
            | Buff of Buff
            | Debuff of Buff
            | Heal of Buff option
        and BuffType =
            | Job of (Job -> Job)
            | Skill of (Skill -> Skill)
            | BuffDuration of (Buff -> (Buff * int * int) option)
        and Buff =
            {
                Name        : string
                Effect      : BuffType
                Duration    : int
                Condition   : Condition
                Stacks      : int
                EndEffect   : (Buff -> Buff) option
                ID          : int
            }
        and Condition =
            | NotLimited
            | LimitedUses of uses:int * contidion:(Skill * Job * Buff list -> bool)
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
                Condition       : (Buff list -> bool) option
                ID              : int
            }
        and Combo =
            {
                Name            : string
                Target          : string
                Effect          : Skill -> Skill
                DistruptedByGCD : bool
                NotFirst        : bool
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

        type GCD =
            | Cooldown of int
            | Available
        
        [<CustomEquality; NoComparison>]
        type Tick =
            {
                ActiveBuffs         : (Buff * int) list
                ActiveDebuffs       : (Buff * int) list
                OGCDTimers          : (Skill * int) list
                GCDtick             : GCD
                ActiveSkill         : Skill option
                ActiveDoTs          : (Skill * (int * int)) list
                ActiveCombo         : (Combo * int) option
            }

            override l.Equals r =
                match r with
                | :? Tick as r ->
                    let lBuffs =
                        l.ActiveBuffs
                        |> List.map (fun (b, _) -> b.Name)
                        |> List.sort
                    let rBuffs =
                        r.ActiveBuffs
                        |> List.map (fun (b, _) -> b.Name)
                        |> List.sort
                    let lOGCD = 
                        l.OGCDTimers
                        |> List.map (fun (s, _) -> s.Name)
                        |> List.sort
                    let rOGCD = 
                        r.OGCDTimers
                        |> List.map (fun (s, _) -> s.Name)
                        |> List.sort
                    let lSkill = 
                        match l.ActiveSkill with 
                        | Some s -> s.Name
                        | None -> ""
                    let rSkill = 
                        match r.ActiveSkill with 
                        | Some s -> s.Name
                        | None -> ""
                    let lDots =
                        l.ActiveDoTs
                        |> List.map (fun (s, _) -> s.Name)
                        |> List.sort
                    let rDots =
                        r.ActiveDoTs
                        |> List.map (fun (s, _) -> s.Name)
                        |> List.sort
                    let lCombo = 
                        match l.ActiveCombo with 
                        | Some (s, _) -> s.Name
                        | None -> ""
                    let rCombo = 
                        match r.ActiveCombo with 
                        | Some (s, _) -> s.Name
                        | None -> ""
                    let lGCD =
                        match l.GCDtick with
                        | Available -> 0
                        | Cooldown t -> t
                    let rGCD =
                        match r.GCDtick with
                        | Available -> 0
                        | Cooldown t -> t

                    lBuffs = rBuffs &&
                    lOGCD = rOGCD &&
                    lSkill = rSkill &&
                    lDots = rDots &&
                    lCombo = rCombo &&
                    lGCD = rGCD
                | _ -> false

            override this.GetHashCode () = 
                hash 
                <| (
                    this.ActiveBuffs.Length, 
                    this.OGCDTimers.Length,
                    this.GCDtick,
                    this.ActiveDoTs.Length
                )

        type Rotation = 
            | Rotation of (Tick list) * Job

            static member empty (job: Job) =
                ([{
                    ActiveBuffs = []
                    ActiveDebuffs = []
                    ActiveSkill = None
                    ActiveDoTs = []
                    GCDtick = Available
                    OGCDTimers = []
                    ActiveCombo = None
                }], job)
                |> Rotation

            static member private updateJob (job: Job) (lastindex: int) (last: Tick) =
                let buffs =
                    last.ActiveBuffs
                    |> List.fold (fun (s: Job -> Job) (v, _) ->
                        match v.Effect with
                        | Skill _ -> id >> s
                        | BuffDuration _ -> id >> s
                        | Job v -> v >> s
                    ) id
                let job =
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
                        {job with MP = newMP; TP = newTP}
                    else
                        job
                buffs job

            static member private updateJobSkillCost (job: Job) (skill: Skill) =
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

            static member private addToBuffList (b: Buff) (list: (Buff * int) list) (lastindex: int) =
                match List.tryFind (fun ((buff, _): Buff * int) -> b.Name = buff.Name) list with
                | None -> 
                    (b, lastindex + 1 + b.Duration) :: list
                | Some (b, _) -> 
                    list
                    |> List.filter (fun (buff, _) ->
                        b.Name <> buff.Name
                    )
                    |> List.append [(b, lastindex + 1 + b.Duration)]

            static member private addDummy (ticklist: Tick list) (n: int) (job: Job) =
                let rec addDummy ticklist n job =
                    if n > 0 then
                        let ticklist = ticklist |> List.indexed
                        let lastindex, last = ticklist |> List.last
                        let job = Rotation.updateJob job lastindex last
                        let activeBuffs =
                            last.ActiveBuffs
                            |> List.map (fun (buff, tick) ->
                                match buff.Condition with
                                | Condition.LimitedUses (n, _) ->
                                    if n > 0 && tick > lastindex + 1 && buff.Stacks >= 0 then
                                        buff, tick
                                    else
                                        match buff.EndEffect with
                                        | None -> buff, tick
                                        | Some effect -> 
                                            let buff = effect buff
                                            buff, lastindex + 1 + buff.Duration
                                | Condition.NotLimited ->
                                    if tick > lastindex + 1 && buff.Stacks >= 0 then
                                        buff, tick
                                    else
                                        match buff.EndEffect with
                                        | None -> buff, tick
                                        | Some effect -> 
                                            let buff = effect buff
                                            buff, lastindex + 1 + buff.Duration
                            )
                            |> List.filter (fun (buff, tick) ->
                                match buff.Condition with
                                | Condition.LimitedUses (n, _) ->
                                    n > 0 && tick > lastindex + 1 && buff.Stacks >= 0
                                | Condition.NotLimited ->
                                    tick > lastindex + 1 && buff.Stacks >= 0
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
                        let gcd =
                            match last.GCDtick with
                            | Available -> Available
                            | Cooldown t ->
                                if t > lastindex + 1 then
                                    Cooldown t
                                else
                                    Available
                        let nextTick =
                            {
                                ActiveBuffs = activeBuffs
                                ActiveDebuffs = activeDebuffs
                                ActiveSkill = None
                                ActiveDoTs = activeDots
                                GCDtick = gcd
                                OGCDTimers = ogcdTimers
                                ActiveCombo = activeCombo
                            }
                        let ticklist = ticklist |> List.map snd
                        addDummy (List.append ticklist [nextTick]) (n - 1) job
                    else
                        job, ticklist
                addDummy ticklist n job

            static member add (skill: Skill) (rotation: Rotation) =
                match rotation with
                | Rotation (ticks, job) ->
                    match ticks with
                    | [] -> failwith "How o.o"
                    | ticks ->
                        let ticks = ticks |> List.indexed
                        let lastindex, last = ticks |> List.last
                        let job = Rotation.updateJob job lastindex last
                        
                        let gcd =
                            match last.GCDtick with
                            | Available -> last.GCDtick
                            | Cooldown t ->
                                if t > lastindex + 1 then
                                    last.GCDtick
                                else
                                    Available
                        let skill =
                            match last.ActiveCombo with
                            | None -> skill
                            | Some (c, _) -> 
                                if c.Target = skill.Name then c.Effect skill else skill
                        let activeCombo = 
                            match skill.Combo with
                            | Some c -> Some (c, lastindex + 1 + 100)
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
                        let casttime =
                            match skill.CastType with
                            | Instant -> 10
                            | Time d -> d
                        let activeBuffs =
                            last.ActiveBuffs
                            |> List.map (fun (buff, tick) ->
                                match buff.Condition with
                                | Condition.NotLimited -> 
                                    if tick > lastindex + 1 && buff.Stacks >= 0 then
                                        buff, tick
                                    else
                                        match buff.EndEffect with
                                        | None -> buff, tick
                                        | Some effect -> 
                                            let buff = effect buff
                                            buff, lastindex + 1 + buff.Duration
                                | Condition.LimitedUses (uses, _) ->
                                    if uses > 0 && tick > lastindex + 1 && buff.Stacks >= 0 then
                                        buff, tick
                                    else
                                        match buff.EndEffect with
                                        | None -> buff, tick
                                        | Some effect -> 
                                            let buff = effect buff
                                            buff, lastindex + 1 + buff.Duration
                            )
                            |> List.filter (fun (buff, tick) ->
                                match buff.Condition with
                                | Condition.LimitedUses (n, _) ->
                                    n > 0 && tick > lastindex + 1 && buff.Stacks >= 0
                                | Condition.NotLimited ->
                                    tick > lastindex + 1 && buff.Stacks >= 0
                            )
                        let addedActiveBuffs =
                            let activeBuffs =
                                activeBuffs
                                |> List.map (fun (buff, tick) ->
                                    match buff.Condition with
                                    | Condition.NotLimited -> buff, tick
                                    | Condition.LimitedUses (uses, cond) ->
                                        let buffs = activeBuffs |> List.map fst
                                        let buff =
                                            if cond (skill, job, buffs) then
                                                {buff with 
                                                    Condition = Condition.LimitedUses (uses - 1, cond)}
                                            else
                                                buff
                                        buff, tick
                                )
                            match skill.Action with
                            | ActionType.DamageOverTime (_, _, b)
                            | ActionType.Heal b ->
                                match b with
                                | None -> activeBuffs
                                | Some b -> 
                                    match b.Effect with
                                    | BuffDuration buff -> 
                                        activeBuffs
                                        |> List.map (fun (b, i) ->
                                            match buff b with
                                            | None -> b, i
                                            | Some (b, dur, max) ->
                                                if (lastindex + 1) + max < i + dur then
                                                    (b, (lastindex + 1) + max)
                                                else 
                                                    (b, i + dur)
                                        )
                                    | _ -> Rotation.addToBuffList b activeBuffs lastindex
                            | ActionType.Damage b ->
                                match b with
                                | None -> activeBuffs
                                | Some b ->
                                    b
                                    |> List.fold (fun activeBuffs b ->
                                        match b.Effect with
                                        | BuffDuration buff -> 
                                            activeBuffs
                                            |> List.map (fun (b, i) ->
                                                match buff b with
                                                | None -> b, i
                                                | Some (b, dur, max) ->
                                                    if (lastindex + 1) + max < i + dur then
                                                        (b, (lastindex + 1) + max)
                                                    else 
                                                        (b, i + dur)
                                            )
                                        | _ -> Rotation.addToBuffList b activeBuffs lastindex
                                    ) activeBuffs
                            | ActionType.Buff b -> 
                                match b.Effect with
                                    | BuffDuration buff -> 
                                        activeBuffs
                                        |> List.map (fun (b, i) ->
                                            match buff b with
                                            | None -> b, i
                                            | Some (b, dur, max) ->
                                                if (lastindex + 1) + max < i + dur then
                                                    (b, (lastindex + 1) + max)
                                                else 
                                                    (b, i + dur)
                                        )
                                    | _ -> Rotation.addToBuffList b activeBuffs lastindex
                            | _ -> activeBuffs
                        let activeDebuffs =
                            let activeDebuffs =
                                last.ActiveDebuffs
                                |> List.filter (fun (_, tick) ->
                                    tick > lastindex + 1
                                )
                            match skill.Action with
                            | ActionType.Debuff b -> Rotation.addToBuffList b activeDebuffs lastindex
                            | _ -> activeDebuffs
                        let activeDots =
                            let activeDots =
                                last.ActiveDoTs
                                |> List.filter (fun (_, (_, tick)) ->
                                    tick > lastindex + 1
                                )
                            match skill.Action with
                            | ActionType.DamageOverTime (potency, duration, _) ->
                                let buffs =
                                    last.ActiveBuffs
                                    |> List.fold (fun (s: Skill -> Skill) (v, _) ->
                                        match v.Effect with
                                        | BuffType.Skill b -> s >> b
                                        | _ -> id >> s
                                    ) id
                                let debuffs =
                                    last.ActiveDebuffs
                                    |> List.fold (fun (s: Skill -> Skill) (v, _) ->
                                        match v.Effect with
                                        | BuffType.Skill b -> s >> b
                                        | _ -> id >> s
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
                                let skill =
                                    (buffs >> debuffs >> combo) {skill with Potency = potency}
                                match List.tryFind (fun ((s, _): Skill * (int * int)) -> s.Name = skill.Name) activeDots with
                                | None -> 
                                    (skill, (lastindex + 1, lastindex + 1 + duration)) :: activeDots
                                | Some (s, _) -> 
                                    activeDots
                                    |> List.filter (fun (skill, _) ->
                                        s.Name <> skill.Name
                                    )
                                    |> List.append [(skill, (lastindex + 1, lastindex + 1 + duration))]
                            | _ -> activeDots
                        let ogcdTimers =
                            last.OGCDTimers
                            |> List.filter (fun (_, tick) ->
                                tick > lastindex + 1
                            )
                        
                        match skill.CooldownType with
                        | GlobalCooldown ->
                            let addSkill =
                                let job = Rotation.updateJobSkillCost job skill
                                match job with
                                | None -> rotation
                                | Some job ->
                                    let nextTick =
                                        {
                                            ActiveBuffs = addedActiveBuffs
                                            ActiveDebuffs = activeDebuffs
                                            ActiveSkill = Some skill
                                            ActiveDoTs = activeDots
                                            GCDtick = Cooldown (lastindex + 1 + job.Speed)
                                            OGCDTimers = ogcdTimers
                                            ActiveCombo = activeCombo
                                        }
                                    let ticks = ticks |> List.map snd
                                    let job, ticks = Rotation.addDummy (List.append ticks [nextTick]) casttime job
                                    Rotation (ticks, job)
                            let confirmedSkill =
                                match last.GCDtick with
                                | Cooldown t -> 
                                    let dummies = t - lastindex + 1
                                    let ticks = ticks |> List.map snd
                                    let job, ticks = Rotation.addDummy ticks dummies job
                                    Rotation.add skill (Rotation (ticks, job))
                                | Available -> 
                                    addSkill
                            match skill.Condition with
                                | None -> confirmedSkill
                                | Some cond when cond (activeBuffs |> List.map fst) -> confirmedSkill
                                | Some _ -> rotation
                        | OffGlobalCooldown cd ->
                            match List.tryFind (fun ((s, _): Skill * int) -> s.Name = skill.Name) ogcdTimers with
                            | Some (_, tick) -> 
                                if tick < lastindex + 1 + job.Speed then
                                    let ticks = ticks |> List.map snd
                                    let job, ticks = Rotation.addDummy ticks (lastindex + 1 + job.Speed - tick) job
                                    Rotation.add skill (Rotation (ticks, job))
                                else
                                    rotation
                                //let ticks = ticks |> List.map snd
                                //let job, ticks = addDummy ticks (tick - 1 - lastindex) job
                                //Rotation.add skill (Rotation (ticks, job))
                            | None -> 
                                let addSkill =
                                    let job = Rotation.updateJobSkillCost job skill
                                    match job with
                                    | None -> rotation
                                    | Some job ->
                                        let ogcdTimers = (skill, lastindex + 1 + cd) :: ogcdTimers
                                        let nextTick =
                                            {
                                                ActiveBuffs = addedActiveBuffs
                                                ActiveDebuffs = activeDebuffs
                                                ActiveSkill = Some skill
                                                ActiveDoTs = activeDots
                                                GCDtick = gcd
                                                OGCDTimers = ogcdTimers
                                                ActiveCombo = activeCombo
                                            }
                                        let ticks = ticks |> List.map snd
                                        let job, ticks = Rotation.addDummy (List.append ticks [nextTick]) casttime job
                                        Rotation (ticks, job)
                                match skill.Condition with
                                | None -> addSkill
                                | Some cond when cond (activeBuffs |> List.map fst) -> addSkill
                                | Some _ -> rotation
