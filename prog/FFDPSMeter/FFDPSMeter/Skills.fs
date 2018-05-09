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
                        ID = 0
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
                    ID = 1
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
                    ID = 2
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
                    ID = 3
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
                    ID = 4
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
                        ID = 5
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
                    ID = 5
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
                    Combo           = Some [combo]
                    Condition       = None
                    ID = 6
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
                    Combo           = Some [combo]
                    Condition       = None
                    ID = 7
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
                    ID = 8
                }

        module Paladin =
            let Job : Job =
                let AutoAttack : Skill =
                    {
                        Name = "Auto Attack"
                        Potency = 72
                        Action = ActionType.Damage None
                        CooldownType = CooldownType.OffGlobalCooldown 30
                        CostType = CostType.Free
                        CastType = CastType.Instant
                        SkillType = SkillType.Weaponskill
                        Combo = None
                        Condition = None
                        ID = 0
                    }
                {
                    Name = "Paladin"
                    Speed = 25
                    AutoAttack = AutoAttack
                    MaxMP = 8800
                    MP = 8800
                    TP = 1000
                }

            let FastBlade : Skill =
                let comboRiot : Combo =
                    let buff : Buff = 
                        {
                            Name = "Riot Blade"
                            Effect = BuffType.Job (fun j -> 
                                let mp =
                                    if j.MP + (int ((float j.MaxMP) * 0.1)) > j.MaxMP then
                                        j.MaxMP
                                    else
                                        j.MP + (int ((float j.MaxMP) * 0.1))
                                {j with MP = mp}
                            )
                            Duration = 0
                            Condition = Condition.NotLimited
                            Stacks = 0
                            EndEffect = None
                            ID = 4
                        }
                    let comboGoring =
                        {
                            Name = "Riot Blade"
                            Target = "Goring Blade"
                            Effect = (fun s -> {s with Potency = 250; Action = ActionType.DamageOverTime (60, 210, None)})
                            DistruptedByGCD = true
                            NotFirst = false
                        }
                    let comboRoyal =
                        {
                            Name = "Riot Blade"
                            Target = "Royal Authority"
                            Effect = (fun s -> {s with Potency = 360})
                            DistruptedByGCD = true
                            NotFirst = false
                        }
                    {
                        Name = "Fast Blade"
                        Target = "Riot Blade"
                        Effect = (fun s -> {s with Potency = 240; Action = ActionType.Damage (Some [buff]); Combo = Some [comboGoring; comboRoyal]})
                        DistruptedByGCD = true
                        NotFirst = false
                    }
                let comboSavage : Combo = 
                    let combo : Combo = 
                        {
                            Name = "Savage Blade"
                            Target = "Rage of Halone"
                            Effect = (fun s -> {s with Potency = 270})
                            DistruptedByGCD = true
                            NotFirst = false
                        }
                    {
                        Name = "Fast Blade"
                        Target = "Savage Blade"
                        Effect = (fun s -> {s with Potency = 210; Combo = Some [combo]})
                        DistruptedByGCD = true
                        NotFirst = false
                    }
                {
                    Name = "Fast Blade"
                    Potency = 160
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 60
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = Some [comboSavage; comboRiot]
                    Condition = None
                    ID = 1
                }

            let FightOrFlight : Skill =
                let buff : Buff = 
                    {
                        Name = "Fight or Flight"
                        Effect = Skill (fun s -> {s with Potency = int ((float s.Potency) * 1.25)})
                        Duration = 250
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                        ID = 2
                    }
                {
                    Name = "Fight or Flight"
                    Potency = 0
                    Action = ActionType.Buff [buff]
                    CooldownType = CooldownType.OffGlobalCooldown 600
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                    ID = 2
                }

            let SavageBlade : Skill =
                {
                    Name = "Savage Blade"
                    Potency = 100
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 60
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 3
                }

            let RiotBlade : Skill =
                {
                    Name = "Riot Blade"
                    Potency = 100
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 60
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 4
                }

            let ShieldLob : Skill =
                {
                    Name = "Shield Lob"
                    Potency = 120
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 120
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 5
                }

            let ShieldBash : Skill =
                {
                    Name = "Shield Bash"
                    Potency = 120
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 120
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 6
                }

            let RageOfHalone : Skill = 
                {
                    Name = "Rage of Halone"
                    Potency = 100
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 60
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 7
                }

            let ShieldSwipe : Skill =
                let buff : Buff =
                    {
                        Name = "Blocked"
                        Effect = BuffDuration (fun b ->
                            if b.Name = "Blocked" then
                                Some (b, 0, 0)
                            else
                                None
                        )
                        Duration = 0
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                        ID = 8
                    }
                let cond (buffs: Buff list) =
                    buffs
                    |> List.map (fun b -> b.Name)
                    |> List.contains "Blocked"
                {
                    Name = "Shield Swipe"
                    Potency = 100
                    Action = ActionType.Damage (Some [buff])
                    CooldownType = CooldownType.OffGlobalCooldown 150
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = Some cond
                    ID = 8
                }

            let ShieldOath : Skill =
                let oath : Buff =
                    {
                        Name = "Oath"
                        Effect = Skill id
                        Duration = 6000
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                        ID = 100
                    }
                let buff : Buff =
                    let cond ((_, _, buffs): Skill * Job * Buff list) : bool =
                        let swo =
                            buffs
                            |> List.map (fun b -> b.Name)
                            |> List.contains "Sword Oath"
                        let sho =
                            buffs
                            |> List.map (fun b -> b.Name)
                            |> List.contains "Shield Oath"
                        swo || sho
                    {
                        Name = "Shield Oath"
                        Effect = Skill (fun s -> {s with Potency = int ((float s.Potency) * 0.75)})
                        Duration = 6000
                        Condition = Condition.LimitedUses (1, cond)
                        Stacks = 0
                        EndEffect = None
                        ID = 8
                    }
                {
                    Name = "Shield Oath"
                    Potency = 0
                    Action = ActionType.Buff [buff;oath]
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.MP 600
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                    ID = 8
                }

            let SwordOath : Skill =
                let oath : Buff =
                    {
                        Name = "Oath"
                        Effect = Skill id
                        Duration = 6000
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                        ID = 100
                    }
                let buff : Buff =
                    let cond ((_, _, buffs): Skill * Job * Buff list) : bool =
                        let swo =
                            buffs
                            |> List.map (fun b -> b.Name)
                            |> List.contains "Sword Oath"
                        let sho =
                            buffs
                            |> List.map (fun b -> b.Name)
                            |> List.contains "Shield Oath"
                        swo || sho
                    let addToOath : Buff =
                        {
                            Name = "Oath"
                            Effect = BuffDuration (fun buff ->
                                if buff.Name = "Oath" then
                                    if buff.Stacks + 5 > 100 then
                                        Some ({buff with Stacks = 100; ID = 200}, 6000, 6000)
                                    else
                                        Some ({buff with Stacks = buff.Stacks + 5; ID = buff.ID + 5}, 6000, 6000)
                                else
                                    None
                            )
                            Duration = 0
                            Condition = Condition.NotLimited
                            Stacks = 0
                            EndEffect = None
                            ID = 100
                        }
                    {
                        Name = "Sword Oath"
                        Effect = Skill (fun s -> 
                            if s.Name = "Auto Attack" then
                                {s with Potency = s.Potency + 75; Action = ActionType.Damage (Some [addToOath])}
                            else
                                s
                        )
                        Duration = 6000
                        Condition = Condition.LimitedUses (1, cond)
                        Stacks = 0
                        EndEffect = None
                        ID = 9
                    }
                {
                    Name = "Sword Oath"
                    Potency = 0
                    Action = ActionType.Buff [buff; oath]
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.MP 600
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                    ID = 9
                }
                
            let SpiritsWithin : Skill =
                {
                    Name = "Spirits Within"
                    Potency = 300
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.OffGlobalCooldown 300
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                    ID = 10
                }
                
            let TotalEclipse : Skill =
                {
                    Name = "Total Eclipse"
                    Potency = 110
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 110
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 11
                }

            let CircleOfScorn : Skill =
                {
                    Name = "Circle of Scorn"
                    Potency = 100
                    Action = ActionType.DamageOverTime (30, 150, None)
                    CooldownType = CooldownType.OffGlobalCooldown 250
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                    ID = 12
                }

            let Sheltron : Skill =
                let cond (buffs: Buff list) : bool =
                    buffs
                    |> List.tryFind (fun buff ->
                        buff.Name = "Oath" && buff.Stacks >= 50
                    )
                    |> Option.isSome
                let buff : Buff =
                    let cond ((_, _, buffs): Skill * Job * Buff list) : bool =
                        buffs
                        |> List.map (fun b -> b.Name)
                        |> List.contains "Shield Oath"
                    {
                        Name = "Blocked"
                        Effect = Skill id
                        Duration = 100
                        Condition = Condition.LimitedUses (1, cond)
                        Stacks = 0
                        EndEffect = None
                        ID = 13
                    }
                let removeOath : Buff =
                    {
                        Name = "Oath"
                        Effect = BuffDuration (fun buff ->
                            if buff.Name = "Oath" then
                                Some ({buff with Stacks = buff.Stacks - 50; ID = buff.ID - 50}, 0, 6000)
                            else
                                None
                        )
                        Duration = 0
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                        ID = 0
                    }
                {
                    Name = "Sheltron"
                    Potency = 100
                    Action = ActionType.Buff [buff; removeOath]
                    CooldownType = CooldownType.OffGlobalCooldown 50
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = Some cond
                    ID = 13
                }

            let GoringBlade : Skill =
                {
                    Name = "Goring Blade"
                    Potency = 100
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 50
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 14
                }

            let RoyalAuthority : Skill =
                {
                    Name = "Royal Authority"
                    Potency = 100
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 60
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 15
                }

            let HolySpirit : Skill =
                let addToOath : Buff =
                    {
                        Name = "Oath"
                        Effect = BuffDuration (fun buff ->
                            if buff.Name = "Oath" then
                                if buff.Stacks + 20 > 100 then
                                    Some ({buff with Stacks = 100; ID = 200}, 6000, 6000)
                                else
                                    Some ({buff with Stacks = buff.Stacks + 20; ID = buff.ID + 20}, 6000, 6000)
                            else
                                None
                        )
                        Duration = 0
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                        ID = 100
                    }
                {
                    Name = "Holy Spirit"
                    Potency = 380
                    Action = ActionType.Damage (Some [addToOath])
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.MP 1440
                    CastType = CastType.Time 15
                    SkillType = SkillType.Spell
                    Combo = None
                    Condition = None
                    ID = 16
                }

            let Requiescat : Skill =
                let buff : Buff =
                    {
                        Name = "Requiescat"
                        Effect = Skill (fun s -> 
                            match s.SkillType with
                            | Spell ->
                                {s with Potency = int ((float s.Potency) * 1.2)}
                            | _ -> 
                                s
                        )
                        Duration = 120
                        Condition = Condition.LimitedUses (2, (fun (s, j, b) ->
                            let req =
                                b
                                |> List.tryFind (fun b -> b.Name = "Requiescat")
                            match req with
                            | None -> true
                            | Some buff ->
                                match buff.Condition with
                                | NotLimited -> true
                                | LimitedUses (i, _) ->
                                    if i = 2 && j.MP > int ((float j.MaxMP) * 0.8) then
                                        true
                                    else
                                        false
                        ))
                        Stacks = 0
                        EndEffect = None
                        ID = 17
                    }
                let MPcheck : Buff =
                    {
                        Name = "Requiescat MP Check"
                        Effect = BuffDuration (fun b -> 
                            if b.Name = "Requiescat" then
                                match b.Condition with
                                | NotLimited -> None
                                | LimitedUses (i, _) ->
                                    if i = 1 then
                                        Some (b, 0, 120)
                                    else
                                        Some (b, 0, 0)
                            else
                                None
                        )
                        Duration = 1
                        Condition = Condition.NotLimited
                        Stacks = 1
                        EndEffect = None
                        ID = 18
                    }
                {
                    Name = "Requiescat"
                    Potency = 350
                    Action = ActionType.Damage (Some [buff;MPcheck])
                    CooldownType = CooldownType.OffGlobalCooldown 600
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                    ID = 17
                }


            let Skillset : Skill list =
                [
                    FastBlade
                    FightOrFlight
                    SavageBlade
                    RiotBlade
                    ShieldLob
                    ShieldBash
                    RageOfHalone
                    ShieldSwipe
                    ShieldOath
                    SwordOath
                    SpiritsWithin
                    TotalEclipse
                    CircleOfScorn
                    Sheltron
                    GoringBlade
                    RoyalAuthority
                    HolySpirit
                    Requiescat
                ]

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
                        ID = 0
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
                    let c : Combo =
                        let cond =
                            fun ((s, _, bs): Skill * Job * Buff list) ->
                                match s.SkillType with
                                | Spell
                                | Weaponskill -> true
                                | Ability -> false
                        let buff : Buff =
                            let wbuff : Buff =
                                {
                                    Name = "Enhanced Wheeling Thrust"
                                    Effect = Skill id
                                    Duration = 100
                                    Condition = Condition.LimitedUses (1, cond)
                                    Stacks = 0
                                    EndEffect = None
                                    ID = 10
                                }
                            {
                                Name = "Sharper Fang and Claw"
                                Effect = Skill (fun s ->
                                    if s.Name = "Fang and Claw" then
                                        {s with Action = ActionType.Damage (Some [wbuff])}
                                    else
                                        s
                                )
                                Duration = 100
                                Condition = Condition.LimitedUses (1, cond)
                                Stacks = 0
                                EndEffect = None
                                ID = 7
                            }
                        {
                            Name = "Vorpal Thrust"
                            Target = "Full Thrust"
                            Effect = (fun s -> {s with Potency = 450; Action = ActionType.Damage (Some [buff])})
                            DistruptedByGCD = true
                            NotFirst = true
                        }
                    {
                        Name = "True Thrust"
                        Target = "Vorpal Thrust"
                        Effect = (fun s -> {s with Potency = 250; Combo = Some [c]})
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
                    Combo = Some [combo]
                    Condition = None
                    ID = 1
                }

            let VorpalThrust : Skill =
                {
                    Name = "Vorpal Thrust"
                    Potency = 100
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 50
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 2
                }

            let ImpulseDrive : Skill =
                let combo : Combo =
                    let combo : Combo =
                        let cond =
                            fun ((s, _, _): Skill * Job * Buff list) ->
                                match s.SkillType with
                                | Spell
                                | Weaponskill -> true
                                | Ability -> false
                        let buff : Buff =
                            let fcond =
                                fun ((s, _, bs): Skill * Job * Buff list) ->
                                    match s.SkillType with
                                    | Spell
                                    | Weaponskill -> true
                                    | Ability -> false
                            let fbuff : Buff =
                                {
                                    Name = "Sharper Fang and Claw"
                                    Effect = Skill id
                                    Duration = 100
                                    Condition = Condition.LimitedUses (1, fcond)
                                    Stacks = 0
                                    EndEffect = None
                                    ID = 7
                                }
                            {
                                Name = "Enhanced Wheeling Thrust"
                                Effect = Skill (fun s ->
                                        if s.Name = "Wheeling Thrust" then
                                            {s with Action = ActionType.Damage (Some [fbuff])}
                                        else
                                            s
                                    )
                                Duration = 100
                                Condition = Condition.LimitedUses (1, cond)
                                Stacks = 0
                                EndEffect = None
                                ID = 10
                            }
                        {
                            Name = "Disembowel"
                            Target = "Chaos Thrust"
                            Effect = (fun s -> {s with Potency = 280; Action = ActionType.DamageOverTime (35, 300, Some buff)})
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
                            ID = 9
                        }
                    {
                        Name = "Impulse Drive"
                        Target = "Disembowel"
                        Effect = (fun s -> {s with Potency = 240; Combo = Some [combo]; Action = ActionType.Damage (Some [buff])})
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
                    Combo = Some [combo]
                    Condition = None
                    ID = 3
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
                        ID = 4
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
                    ID = 4
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
                    ID = 5
                }

            let LifeSurge : Skill = // TODO: IMPLEMENT CRIT
                let cond = 
                    fun ((s, _, _): Skill * Job * Buff list) -> 
                        match s.SkillType with
                        | Spell
                        | Ability -> false
                        | Weaponskill -> true
                let buff : Buff =
                    {
                        Name = "Heavy Thrust"
                        Effect = 
                            Skill (fun s -> 
                                match s.SkillType with
                                | SkillType.Spell
                                | SkillType.Ability -> s
                                | SkillType.Weaponskill ->
                                    if s.Name <> "Auto Attack" then
                                        {s with Potency = s.Potency * 2}
                                    else
                                        s
                            )
                        Duration = 100
                        Condition = Condition.LimitedUses (1, cond)
                        Stacks = 0
                        EndEffect = None
                        ID = 6
                    }
                {
                    Name = "Life Surge"
                    Potency = 0
                    Action = ActionType.Buff [buff]
                    CooldownType = CooldownType.OffGlobalCooldown 500
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                    ID = 6
                }

            let FullThrust : Skill =
                {
                    Name = "Full Thrust"
                    Potency = 100
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 50
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 7
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
                        ID = 8
                    }
                {
                    Name = "Blood for Blood"
                    Potency = 0
                    Action = ActionType.Buff [buff]
                    CooldownType = CooldownType.OffGlobalCooldown 800
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                    ID = 8
                }

            let Disembowel : Skill =
                {
                    Name = "Disembowel"
                    Potency = 100
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 50
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 9
                }

            let ChaosThrust : Skill =
                {
                    Name = "Chaos Thrust"
                    Potency = 140
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 50
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 10
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
                        ID = 11
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
                    ID = 11
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
                        ID = 11
                    }
                {
                    Name = "Spineshatter Dive"
                    Potency = 210
                    Action = ActionType.Damage (Some [buff])
                    CooldownType = CooldownType.OffGlobalCooldown 600
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                    ID = 12
                }

            let DoomSpike : Skill =
                let combo : Combo =
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
                            ID = 30
                        }
                    {
                        Name = "Doom Spike"
                        Target = "Sonic Thrust"
                        Effect = (fun (s: Skill) -> {s with Potency = 180; Action = ActionType.Damage (Some [buff])})
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
                    Combo = Some [combo]
                    Condition = None
                    ID = 13
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
                    ID = 14
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
                        ID = 15
                    }
                {
                    Name = "Battle Litany"
                    Potency = 0
                    Action = ActionType.Buff [buff]
                    CooldownType = CooldownType.OffGlobalCooldown 1800
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                    ID = 15
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
                        ID = 30
                    }
                {
                    Name = "Blood of the Dragon"
                    Potency = 0
                    Action = ActionType.Buff [buff]
                    CooldownType = CooldownType.OffGlobalCooldown 1800
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = Some cond
                    ID = 30
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
                        ID = 30
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
                    ID = 17
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
                        ID = 30
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
                    ID = 18
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
                                            {b with Name = "Blood of the Dragon"; ID = 30}
                                        )
                                        ID = 40
                                    }
                                Some (lotd, 0, 300)
                            else
                                None
                        )
                        Duration = 200
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                        ID = 17
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
                    ID = 19
                }

            let SonicThrust : Skill =
                {
                    Name = "Sonic Thrust"
                    Potency = 100
                    Action = ActionType.Damage None
                    CooldownType = CooldownType.GlobalCooldown
                    CostType = CostType.TP 100
                    CastType = CastType.Instant
                    SkillType = SkillType.Weaponskill
                    Combo = None
                    Condition = None
                    ID = 20
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
                        ID = 21
                    }
                {
                    Name = "Dragon Sight"
                    Potency = 0
                    Action = ActionType.Buff [buff]
                    CooldownType = CooldownType.OffGlobalCooldown 1200
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                    ID = 21
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
                                let id = if b.Name = "Blood of the Dragon" then 30 + stacks else 40 + stacks
                                Some ({b with Stacks = stacks; ID = id}, 0, 300)
                            else
                                None
                        )
                        Duration = 0
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                        ID = 22
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
                    ID = 22
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
                    ID = 23
                }

            let Invigorate : Skill =
                let buff : Buff =
                    {
                        Name = "Invigorate"
                        Effect = 
                            BuffType.Job (fun j -> 
                                let tp =
                                    if j.TP + 400 > 1000 then 1000 else j.TP + 400
                                {j with TP = tp}
                            )
                        Duration = 0
                        Condition = Condition.NotLimited
                        Stacks = 0
                        EndEffect = None
                        ID = 24
                    }
                {
                    Name = "Invigorate"
                    Potency = 0
                    Action = ActionType.Buff [buff]
                    CooldownType = CooldownType.OffGlobalCooldown 1200
                    CostType = CostType.Free
                    CastType = CastType.Instant
                    SkillType = SkillType.Ability
                    Combo = None
                    Condition = None
                    ID = 24
                }

            let Skillset : Skill list = 
                [
                    BattleLitany
                    BloodForBlood
                    BloodOfTheDragon
                    ChaosThrust
                    Disembowel
                    DoomSpike
                    DragonfireDive
                    DragonSight
                    FangAndClaw
                    FullThrust
                    Geirskogul
                    HeavyThrust
                    ImpulseDrive
                    Invigorate
                    Jump
                    LifeSurge
                    MirageDive
                    Nastrond
                    PiercingTalon
                    SonicThrust
                    SpineshatterDive
                    TrueThrust
                    VorpalThrust
                    WheelingThrust
                ]
