namespace FFDPSMeter

    module Calculations =
        open Model

        let ToRotation (skills: Skill list) (job: Job) : Rotation =
            skills
            |>
            List.fold (fun rotation skill ->
                Rotation.add skill rotation
            ) (Rotation.empty job)

