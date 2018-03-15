namespace FFDPSMeter

    module Skills =
        open Model

        module Dragoon =

            let Job : Job =
                {
                    Name = "Dragoon"
                    Speed = 25
                    AutoAttack = 80
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
                }

            let GCDWithBuff : Skill =
                let buff =
                    {
                        Name = "GCD Buff"
                        Effect = Skill (fun (s: Skill) -> {s with Potency = int ((float s.Potency) * 1.1)})
                        Duration = 40
                    }
                {
                    Name            = "Damage with buff Skill"
                    Potency         = 40
                    Action          = ActionType.Damage (Some buff)
                    CooldownType    = CooldownType.GlobalCooldown
                    CostType        = CostType.TP 40
                    CastType        = CastType.Instant
                    SkillType       = SkillType.Weaponskill
                }


