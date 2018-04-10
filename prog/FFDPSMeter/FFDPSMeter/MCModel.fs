namespace FFDPSMeter

    module MCModel =
        open Model
        open Calculations

        open System.Collections.Generic

        type State = Tick
        type Action = Skill

        let MC (job: Job) (skillSet: Skill list) (time: int) (episodes: int) (seed: int option) =
            printf "-"
            let random = 
                match seed with
                | Some s -> System.Random s
                | None -> System.Random ()
            let s0 = Rotation.empty job
            let s = 
                match s0 with
                | Rotation (state, _) -> List.head state
            let b =
                let p = 1. / float skillSet.Length
                let ss =
                    skillSet
                    |> List.map (fun s ->
                        s, p
                    )
                dict [s, ss]
            let pi = 
                let p = 1. / float skillSet.Length
                let ss =
                    skillSet
                    |> List.map (fun s ->
                        s, p
                    )
                dict [s, ss]
            printf "-"
            let getAction (s: State) (pi: IDictionary<Tick, (Skill * float) list>) =
                let rand = random.Next(0, 1000)
                let skillset =
                    match pi.TryGetValue s with
                    | true, value -> value
                    | false, _ ->
                        let p = 1. / float skillSet.Length
                        let ss =
                            skillSet
                            |> List.map (fun s ->
                                s, p
                            )
                        pi.Add (s, ss)
                        ss
                skillset
                |> List.fold (fun (prob, (ss: (float * float * Action) list)) (a, p) ->
                    let ap = p * 1000.
                    let probU = prob + ap
                    probU, ((prob, probU, a) :: ss)
                ) (0., [])
                |> snd
                |> List.find (fun (p, pU, _) ->
                    int p < rand && rand < int pU
                )
                |> fun (_, _, a) -> a

            Seq.init episodes (fun n -> n + 1)
            |> Seq.iter (fun n ->
                let rec addToRotation (r: Rotation) =
                    printf "-"
                    match r with
                    | Rotation (ticks, _) ->
                        if ticks.Length > time then
                            r
                        else
                            let last = List.last ticks
                            addToRotation (Rotation.add (getAction last b) r)
                let rot = addToRotation (Rotation.empty job)
                match rot with
                | Rotation (rotation, job) ->
                    rotation
                    |> List.indexed
                    |> List.iter (fun (i, s) ->
                        let combo =
                            match s.ActiveCombo with
                            | None -> "None"
                            | Some (c, _) -> sprintf "%s to %s" c.Name c.Target
                        match s.ActiveSkill with
                        | None -> ()
                        | Some s -> printf "%d:\tSkill: %20s\tPotency: %d\tCombo: %s\n" i s.Name s.Potency combo
                    )
                    printfn "MP: %d, TP: %d" job.MP job.TP
                printfn "DPS: %d" (ToDPS rot false)
            )

    module MCModel2 =

        open Model
        open Calculations

        type State = 
            (string list) * (string list) * GCD * (string list) * (string option) * int * int

        let fromRotation (r: Rotation) : State =
            match r with
            | Rotation (ticks, j) ->
                let last = List.last ticks
                let buffs =
                    last.ActiveBuffs
                    |> List.map (fun (b, _) -> b.Name)
                    |> List.sort
                let ogcd =
                    last.OGCDTimers
                    |> List.map (fun (b, _) -> b.Name)
                    |> List.sort
                let dots =
                    last.ActiveDoTs
                    |> List.map (fun (b, _) -> b.Name)
                    |> List.sort
                let combo =
                    last.ActiveCombo
                    |> Option.map (fun (c, _) -> c.Target)
                (buffs, ogcd, last.GCDtick, dots, combo, ((j.TP / 100) * 100), ((j.MP / 100) * 100))

        let createRandomPolicy (n: int) =
            let ones = Seq.init n (fun _ -> 1. / float n)
            fun (s: State) -> ones

        let createGreedyPolicy (Q:Map<State, float seq>) (n: int) =
            let policy (s: State) =
                let values =
                    match Q.TryFind s with
                    | Some values -> values
                    | None -> Seq.init n (fun _ -> 0.)
                let A = Seq.init (Seq.length values) (fun _ -> 0.)
                let best = 
                    values
                    |> List.ofSeq
                    |> List.indexed
                    |> List.maxBy snd
                    |> fst
                A
                |> Seq.indexed
                |> Seq.map (fun (i, v) -> 
                    if i = best then 1. else v
                )
            policy

        let genEpisode (behavior: State -> float seq) (random: System.Random) (job: Job * Skill seq) (parseDuration: int) =
            let rot = Rotation.empty (fst job)
            let state = fromRotation rot
            let rec genEpisode (rot: Rotation) (state: State) (finished: bool) (ep: (State * int * float) seq) =
                if finished then
                    ep
                else
                    let probs = behavior state 
                    let action = 
                        let rand = random.Next(0, 1000)
                        probs
                        |> Seq.indexed
                        |> Seq.fold (fun (prob, probs) (i, p) ->
                            let ap = p * 1000.
                            let probU = prob + ap
                            probU, ((prob, probU, i) :: probs)
                        ) (0., [])
                        |> snd
                        |> List.find (fun (p, pU, _) ->
                            int p <= rand && rand < int pU
                        )
                        |> fun (_, _, a) -> a
                    let nextState, reward, finished, nextRotation =
                        let skill = 
                            (snd job)
                            |> Seq.item action
                        printfn "%s" skill.Name
                        let nextRot = Rotation.add skill rot
                        let nextState = fromRotation nextRot
                        let reward = float <| ToDPS nextRot false
                        let finished =
                            match nextRot with
                            | Rotation (ticks, _) -> List.length ticks > parseDuration
                        nextState, reward, finished, nextRot
                    let episode = Seq.append ep [(state, action, reward)]
                    genEpisode nextRotation nextState finished episode
            genEpisode rot state false []
            
        let mcControlImportanceSampling (job: Job * Skill seq) (parseDuration: int) (env: State) (episodes: int) (behavior: State -> float seq) (seed: int option) =
            let Q : Map<State, float seq> = Map.empty
            let C : Map<State, float seq> = Map.empty
            let nA = job |> snd |> Seq.length
            let random =
                match seed with
                | None -> System.Random()
                | Some s -> System.Random s

            let targetPolicy = createGreedyPolicy Q nA
            
            Seq.init episodes id
            |> Seq.fold (fun s ep ->
                if ep % 1 = 0 then printfn "%d" ep
                let Q, C, targetPolicy = s
                let rotation = Rotation.empty (fst job)
                
                let episode = genEpisode behavior random job parseDuration
                let G = 0.
                let W = 1.

                let G, C, Q, W =
                    (G, C, Q, W)
                    |> Seq.foldBack (fun v s ->
                        let state, action, reward = v
                        let G, (C: Map<State, float seq>), (Q: Map<State, float seq>), W = s
                        let G = G + reward
                        let newCVal = 
                            match C.TryFind state with
                            | Some s -> 
                                s
                                |> Seq.indexed
                                |> Seq.map (fun (i, p) ->
                                    if i = action then p + W else p
                                )
                            | None -> 
                                Seq.init nA (fun _ -> 0.)
                                |> Seq.indexed
                                |> Seq.map (fun (i, p) ->
                                    if i = action then p + W else p
                                )
                        let C = C.Add (state, newCVal)
                        let cVal =
                            match C.TryFind state with
                            | Some s -> 
                                s |> Seq.item action
                            | None -> 0.
                        let newQVal =
                            match Q.TryFind state with
                            | Some s -> 
                                s
                                |> Seq.indexed
                                |> Seq.map (fun (i, p) ->
                                    if i = action then p + (W / cVal) * (G - p) else p
                                )
                            | None -> 
                                Seq.init nA (fun _ -> 0.)
                                |> Seq.indexed
                                |> Seq.map (fun (i, p) ->
                                    if i = action then (W / cVal) * G else 0.
                                )
                        let Q = Q.Add (state, newQVal)
                        let denom = 
                            behavior state
                            |> Seq.item action
                        let W = W * 1./denom
                        G, C, Q, W
                    ) episode
                Q, C, targetPolicy
            ) (Q, C, targetPolicy)

        let runPolicy (job: Job * Skill seq) (behavior: State -> float seq) (parseDuration: int) (seed: int option) =
            let rotation = Rotation.empty (fst job)
            let random =
                match seed with
                | None -> System.Random()
                | Some s -> System.Random s
                
            printfn "lul"
            let episode = genEpisode behavior random job parseDuration
            printfn "lul"
            let rot =
                episode
                |> Seq.map (fun (_, i, _) -> Seq.item i (snd job))
                |> Seq.fold (fun s v ->
                    printfn "%s" v.Name
                    Rotation.add v s
                ) rotation
            printfn "DPS: %d" (ToDPS rot true)

