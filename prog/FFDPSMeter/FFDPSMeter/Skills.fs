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
                }

            let TestDoT : Skill =
                {
                    Name            = "DoT Skill"
                    Potency         = 40
                    Action          = ActionType.DamageOverTime (120, None)
                    CooldownType    = CooldownType.OffGlobalCooldown 45
                    CostType        = CostType.Free
                    CastType        = CastType.Instant
                    SkillType       = SkillType.Weaponskill
                    Combo           = None
                }

            let GCDWithBuff : Skill =
                let buff =
                    {
                        Name = "GCD Buff"
                        Effect = Skill (fun (s: Skill) -> {s with Potency = int ((float s.Potency) * 1.1)})
                        Duration = 150
                    }
                {
                    Name            = "Damage with buff Skill"
                    Potency         = 40
                    Action          = ActionType.Damage (Some buff)
                    CooldownType    = CooldownType.GlobalCooldown
                    CostType        = CostType.TP 100
                    CastType        = CastType.Instant
                    SkillType       = SkillType.Weaponskill
                    Combo           = None
                }

            let Combo1 : Skill =
                let combo : Combo =
                    {
                        Name = "Combo 1"
                        Target = "Combo 2"
                        Effect = (fun (s: Skill) -> {s with Potency = int ((float s.Potency) * 1.1)})
                        DistruptedByGCD = true
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
                }

            let Combo2 : Skill =
                let combo : Combo =
                    {
                        Name = "Combo 2"
                        Target = "Combo 3"
                        Effect = (fun (s: Skill) -> {s with Potency = s.Potency * 2})
                        DistruptedByGCD = true
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
                }



