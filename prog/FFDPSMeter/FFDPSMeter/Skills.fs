namespace FFDPSMeter

    module Skills =
        open Model

        module TestJob =

            let Job : Job =
                let AutoAttack : Skill =
                    {
                        Name            = "Test Skill"
                        Potency         = 80
                        Action          = ActionType.Damage None
                        CooldownType    = CooldownType.OffGlobalCooldown 0
                        CostType        = CostType.Free
                        CastType        = CastType.Instant
                        SkillType       = SkillType.Weaponskill
                        Combo           = None
                        Condition       = None
                    }
                {
                    Name = "Test Job"
                    Speed = 25
                    AutoAttack = AutoAttack
                    MaxMP = 3400
                    MP = 3400
                    TP = 1000
                }

            let TestSkill : Skill = 
                {
                    Name            = "Test Skill"
                    Potency         = 80
                    Action          = ActionType.Damage None
                    CooldownType    = CooldownType.GlobalCooldown
                    CostType        = CostType.Free
                    CastType        = CastType.Instant
                    SkillType       = SkillType.Weaponskill
                    Combo           = None
                    Condition       = None
                }

            let TestOGCDSkill : Skill = 
                {
                    Name            = "oGCD Skill"
                    Potency         = 175
                    Action          = ActionType.Damage None
                    CooldownType    = CooldownType.OffGlobalCooldown 30
                    CostType        = CostType.Free
                    CastType        = CastType.Instant
                    SkillType       = SkillType.Weaponskill
                    Combo           = None
                    Condition       = None
                }

            let TestOGCDSkill2 : Skill =
                {
                    Name            = "oGCD 2 Skill"
                    Potency         = 120
                    Action          = ActionType.Damage None
                    CooldownType    = CooldownType.OffGlobalCooldown 45
                    CostType        = CostType.Free
                    CastType        = CastType.Instant
                    SkillType       = SkillType.Weaponskill
                    Combo           = None
                    Condition       = None
                }

            let TestDoT : Skill =
                {
                    Name            = "DoT Skill"
                    Potency         = 40
                    Action          = ActionType.DamageOverTime (20, 120, None)
                    CooldownType    = CooldownType.OffGlobalCooldown 45
                    CostType        = CostType.Free
                    CastType        = CastType.Instant
                    SkillType       = SkillType.Weaponskill
                    Combo           = None
                    Condition       = None
                }

            let GCDWithBuff : Skill =
                let buff =
                    {
                        Name = "GCD Buff"
                        Effect = Skill (fun (s: Skill) -> {s with Potency = int ((float s.Potency) * 1.1)})
                        Duration = 150
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name            = "Damage with buff Skill"
                    Potency         = 40
                    Action          = ActionType.Damage (Some [buff])
                    CooldownType    = CooldownType.GlobalCooldown
                    CostType        = CostType.TP 100
                    CastType        = CastType.Instant
                    SkillType       = SkillType.Weaponskill
                    Combo           = None
                    Condition       = None
                }

            let Combo1 : Skill =
                let combo : Combo =
                    {
                        Name = "Combo 1"
                        Target = "Combo 2"
                        Effect = (fun (s: Skill) -> {s with Potency = int ((float s.Potency) * 1.1)})
                        DistruptedByGCD = true
                        NotFirst = false
                    }
                {
                    Name            = "Combo 1"
                    Potency         = 40
                    Action          = ActionType.Damage None
                    CooldownType    = CooldownType.GlobalCooldown
                    CostType        = CostType.TP 100
                    CastType        = CastType.Instant
                    SkillType       = SkillType.Weaponskill
                    Combo           = Some combo
                    Condition       = None
                }

            let Combo2 : Skill =
                let combo : Combo =
                    {
                        Name = "Combo 2"
                        Target = "Combo 3"
                        Effect = (fun (s: Skill) -> {s with Potency = s.Potency * 2})
                        DistruptedByGCD = true
                        NotFirst = false
                    }
                {
                    Name            = "Combo 2"
                    Potency         = 70
                    Action          = ActionType.Damage None
                    CooldownType    = CooldownType.GlobalCooldown
                    CostType        = CostType.TP 100
                    CastType        = CastType.Instant
                    SkillType       = SkillType.Weaponskill
                    Combo           = Some combo
                    Condition       = None
                }
                
            let Combo3 : Skill =
                {
                    Name            = "Combo 3"
                    Potency         = 100
                    Action          = ActionType.Damage None
                    CooldownType    = CooldownType.GlobalCooldown
                    CostType        = CostType.TP 100
                    CastType        = CastType.Instant
                    SkillType       = SkillType.Weaponskill
                    Combo           = None
                    Condition       = None
                }

        module Dragoon =
            let Job : Job =
                let AutoAttack : Skill =
                    {
                        Name = "Auto Attack"
                        Potency = 98
                        Action = ActionType.Damage None
                        CooldownType = CooldownType.OffGlobalCooldown 30
                        CostType = CostType.Free
                        CastType = CastType.Instant
                        SkillType = SkillType.Weaponskill
                        Combo = None
                        Condition = None
                    }
                {
                    Name = "Dragoon"
                    Speed = 24
                    AutoAttack = AutoAttack
                    MaxMP = 5880
                    MP = 5880
                    TP = 1000
                }

            let TrueThrust : Skill =
                let combo : Combo =
                    {
                        Name = "True Thrust"
                        Target = "Vorpal Thrust"
                        Effect = (fun s -> {s with Potency = 250})
                        DistruptedByGCD = true
                        NotFirst = false
                    }
                {
                    Name = "True Thrust"
                    Potency = 160
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 60
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = Some combo
                    Condition = None
                }

            let VorpalThrust : Skill =
                let combo : Combo =
                    {
                        Name = "Vorpal Thrust"
                        Target = "Full Thrust"
                        Effect = (fun s -> {s with Potency = 450})
                        DistruptedByGCD = true
                        NotFirst = true
                    }
                {
                    Name = "Vorpal Thrust"
                    Potency = 100
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 50
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = Some combo
                    Condition = None
                }

            let ImpulseDrive : Skill =
                let combo : Combo =
                    {
                        Name = "Impulse Drive"
                        Target = "Disembowel"
                        Effect = (fun s -> {s with Potency = 240})
                        DistruptedByGCD = true
                        NotFirst = false
                    }
                {
                    Name = "Impulse Drive"
                    Potency = 200
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 60
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = Some combo
                    Condition = None
                }

            let HeavyThrust : Skill =
                let buff : Buff =
                    {
                        Name = "Heavy Thrust"
                        Effect = Skill (fun s -> {s with Potency = int ((float s.Potency) * 1.1)})
                        Duration = 300
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Heavy Thrust"
                    Potency = 190
                    Action = ActionType.Damage (Some [buff])
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 60
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                }

            let PiercingTalon : Skill =
                {
                    Name = "Piercing Talon"
                    Potency = 120
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 130
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                }

            let LifeSurge : Skill = // TODO: IMPLEMENT CRIT
                let cond = 
                    fun ((s, _, _): Skill * Job * Buff list) -> 
                        match s.SkillType with
                        | Ability -> false
                        | Weaponskill -> true
                let buff : Buff =
                    {
                        Name = "Heavy Thrust"
                        Effect = 
                            Skill (fun s -> 
                                match s.SkillType with
                                | SkillType.Ability -> s
                                | SkillType.Weaponskill ->
                                    {s with Potency = s.Potency * 2}
                            )
                        Duration = 100
                        Condition = Condition.LimitedUses (1, cond)
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Life Surge"
                    Potency = 0
                    Action = ActionType.Buff buff
                    CooldownType = CooldownType.OffGlobalCooldown 500
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                }

            let FullThrust : Skill =
                let cond =
                    fun ((s, _, bs): Skill * Job * Buff list) ->
                        match s.SkillType with
                        | Weaponskill -> true
                        | Ability -> false
                let buff : Buff =
                    {
                        Name = "Sharper Fang and Claw"
                        Effect = Skill id
                        Duration = 100
                        Condition = Condition.LimitedUses (1, cond)
                        Stacks = 0
                        EndEffect = Some (fun (b: Buff) ->
                            {b with 
                                Name = "Enhanced Wheeling Thrust"
                                Condition = Condition.LimitedUses (1, cond)
                                EndEffect = None}
                        )
                    }
                {
                    Name = "Full Thrust"
                    Potency = 100
                    Action = ActionType.Damage (Some [buff])
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 50
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                }

            let BloodForBlood : Skill =
                let buff : Buff =
                    {
                        Name = "Blood for Blood"
                        Effect = Skill (fun s -> {s with Potency = int ((float s.Potency) * 1.15)})
                        Duration = 200
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Blood for Blood"
                    Potency = 0
                    Action = ActionType.Buff buff
                    CooldownType = CooldownType.OffGlobalCooldown 800
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                }

            let Disembowel : Skill =
                let combo : Combo =
                    {
                        Name = "Disembowel"
                        Target = "Chaos Thrust"
                        Effect = (fun s -> {s with Potency = 280})
                        DistruptedByGCD = true
                        NotFirst = true
                    }
                let buff : Buff =
                    {
                        Name = "Piercing resistance down"
                        Effect = Skill (fun s -> {s with Potency = int ((float s.Potency) * 1.05)})
                        Duration = 300
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Disembowel"
                    Potency = 100
                    Action = ActionType.Damage (Some [buff])
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 50
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = Some combo
                    Condition = None
                }

            let ChaosThrust : Skill =
                let cond =
                    fun ((s, _, _): Skill * Job * Buff list) ->
                        match s.SkillType with
                        | Weaponskill -> true
                        | Ability -> false
                let buff : Buff =
                    {
                        Name = "Enhanced Wheeling Thrust"
                        Effect = Skill id
                        Duration = 100
                        Condition = Condition.LimitedUses (1, cond)
                        Stacks = 0
                        EndEffect = Some (fun (b: Buff) ->
                            {b with 
                                Name = "Sharper Fang and Claw"
                                Condition = Condition.LimitedUses (1, cond)
                                EndEffect = None}
                        )
                    }
                {
                    Name = "Chaos Thrust"
                    Potency = 140
                    Action = ActionType.DamageOverTime (35, 300, Some buff)
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 50
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                }

            let Jump : Skill =
                let cond =
                    fun ((s, _, _): Skill * Job * Buff list) ->
                        if s.Name = "Mirage Dive" then
                            true
                        else
                            false
                let buff : Buff =
                    {
                        Name = "Dive Ready"
                        Effect = Skill id
                        Duration = 150
                        Condition = Condition.LimitedUses (1, cond)
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Jump"
                    Potency = 240
                    Action = ActionType.Damage (Some [buff])
                    CooldownType = CooldownType.OffGlobalCooldown 300
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                }

            let SpineshatterDive : Skill =
                let cond =
                    fun ((s, _, _): Skill * Job * Buff list) ->
                        if s.Name = "Mirage Dive" then
                            true
                        else
                            false
                let buff : Buff =
                    {
                        Name = "Dive Ready"
                        Effect = Skill id
                        Duration = 150
                        Condition = Condition.LimitedUses (1, cond)
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Spineshatter Jump"
                    Potency = 210
                    Action = ActionType.Damage (Some [buff])
                    CooldownType = CooldownType.OffGlobalCooldown 600
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                }

            let DoomSpike : Skill =
                let combo : Combo =
                    {
                        Name = "Doom Spike"
                        Target = "Sonic Thrust"
                        Effect = (fun (s: Skill) -> {s with Potency = 180})
                        DistruptedByGCD = true
                        NotFirst = true
                    }
                {
                    Name = "Doom Spike"
                    Potency = 140
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 120
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = Some combo
                    Condition = None
                }

            let DragonfireDive : Skill =
                {
                    Name = "Dragonfire Dive"
                    Potency = 320
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.OffGlobalCooldown 1200
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                }

            let BattleLitany : Skill =
                let buff : Buff =
                    {
                        Name = "Battle Litany"
                        Effect = Skill (fun s -> {s with Potency = int ((float s.Potency) * 1.1)})
                        Duration = 200
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Battle Litany"
                    Potency = 0
                    Action = ActionType.Buff buff
                    CooldownType = CooldownType.OffGlobalCooldown 1800
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                }

            let BloodOfTheDragon : Skill =
                let cond (buffs: Buff list) : bool =
                    buffs
                    |> List.map (fun b -> b.Name)
                    |> List.contains "Life of the Dragon"
                    |> not
                let buff : Buff =
                    {
                        Name = "Blood of the Dragon"
                        Effect = Skill (fun s -> 
                            if s.Name = "Jump" || s.Name = "Spineshatter Dive" then
                                {s with Potency = int ((float s.Potency) * 1.3)}
                            else
                                s
                        )
                        Duration = 200
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Blood of the Dragons"
                    Potency = 0
                    Action = ActionType.Buff buff
                    CooldownType = CooldownType.OffGlobalCooldown 1800
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = Some cond
                }

            let FangAndClaw =
                let cond (buffs: Buff list) : bool =
                    let buffs = buffs |> List.map (fun b -> b.Name)
                    let botd = buffs |> List.contains "Blood of the Dragon"
                    let lotd = buffs |> List.contains "Life of the Dragon"
                    let sharper = buffs |> List.contains "Sharper Fang and Claw"
                    (botd || lotd) && sharper
                let buff : Buff =
                    {
                        Name = "Blood of the Dragon"
                        Effect = BuffDuration (fun b -> 
                            if b.Name = "Blood of the Dragon" then
                                Some (b, 100, 300)
                            else
                                None
                        )
                        Duration = 0
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Fang and Claw"
                    Potency = 300
                    Action = ActionType.Damage (Some [buff])
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 50
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = Some cond
                }

            let WheelingThrust =
                let cond (buffs: Buff list) : bool =
                    let buffs = buffs |> List.map (fun b -> b.Name)
                    let botd = buffs |> List.contains "Blood of the Dragon"
                    let lotd = buffs |> List.contains "Life of the Dragon"
                    let wheeling = buffs |> List.contains "Enhanced Wheeling Thrust"
                    (botd || lotd) && wheeling
                let buff : Buff =
                    {
                        Name = "Blood of the Dragon"
                        Effect = BuffDuration (fun b -> 
                            if b.Name = "Blood of the Dragon" then
                                Some (b, 100, 300)
                            else
                                None
                        )
                        Duration = 0
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Wheeling Thrust"
                    Potency = 300
                    Action = ActionType.Damage (Some [buff])
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 50
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = Some cond
                }

            let Geirskogul : Skill =
                let cond (buffs: Buff list) : bool =
                    buffs
                    |> List.map (fun b -> b.Name)
                    |> List.contains "Blood of the Dragon"
                let buff : Buff =
                    {
                        Name = "Blood of the Dragon"
                        Effect = BuffDuration (fun b -> 
                            if b.Name = "Blood of the Dragon" && b.Stacks = 3 then
                                let lotd =
                                    {b with
                                        Name = "Life of the Dragon"
                                        Stacks = 0
                                        EndEffect = Some (fun b ->
                                            {b with Name = "Blood of the Dragon"}
                                        )
                                    }
                                Some (lotd, 0, 300)
                            else
                                None
                        )
                        Duration = 200
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Geirskogul"
                    Potency = 230
                    Action = ActionType.Damage (Some [buff])
                    CooldownType = CooldownType.OffGlobalCooldown 300
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = Some cond
                }

            let SonicThrust : Skill =
                let buff : Buff =
                    {
                        Name = "Blood of the Dragon"
                        Effect = BuffDuration (fun b -> 
                            if b.Name = "Blood of the Dragon" then
                                Some (b, 100, 300)
                            else
                                None
                        )
                        Duration = 0
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Sonic Thrust"
                    Potency = 100
                    Action = ActionType.Damage (Some [buff])
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 100
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                }

            let DragonSight : Skill =
                let buff : Buff =
                    {
                        Name = "Dragon Sight"
                        Effect = Skill (fun s -> {s with Potency = int ((float s.Potency) * 1.1)})
                        Duration = 200
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Dragon Sight"
                    Potency = 0
                    Action = ActionType.Buff buff
                    CooldownType = CooldownType.OffGlobalCooldown 1200
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                }

            let MirageDive : Skill =
                let cond (buffs: Buff list) : bool =
                    buffs
                    |> List.map (fun b -> b.Name)
                    |> List.contains "Dive Ready"
                let buff : Buff =
                    {
                        Name = "Mirage Dive"
                        Effect = BuffDuration (fun b -> 
                            if b.Name = "Blood of the Dragon" || b.Name = "Life of the Dragon" then
                                let stacks = if b.Stacks = 3 then 3 else b.Stacks + 1
                                Some ({b with Stacks = stacks}, 0, 300)
                            else
                                None
                        )
                        Duration = 0
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                    }
                {
                    Name = "Mirage Dive"
                    Potency = 210
                    Action = ActionType.Damage (Some [buff])
                    CooldownType = CooldownType.OffGlobalCooldown 10
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = Some cond
                }

            let Nastrond : Skill =
                let cond (buffs: Buff list) : bool =
                    buffs 
                    |> List.map (fun b -> b.Name)
                    |> List.contains "Life of the Dragon"
                {
                    Name = "Nastrond"
                    Potency = 330
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.OffGlobalCooldown 100
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = Some cond
                }






