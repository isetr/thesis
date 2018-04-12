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
            (int list) * (int list) * int * (int list) * (string option) * int * int

        let fromRotation (r: Rotation) : State =
            match r with
            | Rotation (ticks, j) ->
                let last = List.last ticks
                let buffs =
                    last.ActiveBuffs
                    |> List.map (fun (b, _) -> b.ID)
                    |> List.sort
                let ogcd =
                    last.OGCDTimers
                    |> List.map (fun (b, _) -> b.ID)
                    |> List.sort
                let dots =
                    last.ActiveDoTs
                    |> List.map (fun (b, _) -> b.ID)
                    |> List.sort
                let combo =
                    last.ActiveCombo
                    |> Option.map (fun (c, _) -> c.Target)
                let gcd = 
                    match last.GCDtick with
                    | Cooldown _ -> 0
                    | Available -> 1
                (buffs, ogcd, gcd, dots, combo, j.TP, j.MP)
        
        //type State = int list

        //let fromRotation (r: Rotation) : State =
        //    match r with
        //    | Rotation (ticks, _) ->
        //        ticks
        //        |> List.map (fun t ->
        //            match t.ActiveSkill with
        //            | None -> -1
        //            | Some s -> s.ID
        //        )
        //        |> List.filter (fun id -> id >= 0)
        //        |> List.rev
        //        |> List.truncate 6
        //        |> List.rev

        let createRandomPolicy (n: int) =
            let ones = List.init n (fun _ -> 1. / float n)
            fun (s: State) -> ones

        let createEpsilonGreedyPolicy (Q: Map<State, float list>) (nA: int) (epsilon: float) =
            let policy (s: State) =
                match Q.TryFind s with
                | Some values -> 
                    let prob = if nA > 1 then epsilon / float (nA - 1) else 1.
                    let A = List.init (List.length values) (fun _ -> prob)
                    let best = 
                        values
                        |> List.indexed
                        |> List.maxBy snd
                        |> fst
                    A
                    |> List.indexed
                    |> List.map (fun (i, v) -> 
                        if i = best then 1. - epsilon else v
                    )
                | None -> 
                    List.init nA (fun _ -> if nA > 1 then 1. / float nA else 1.)
            policy

        let createGreedyPolicy (Q:Map<State, float list>) (n: int) =
            createEpsilonGreedyPolicy Q n 0.

        let genEpisode (behavior: State -> float list) (random: System.Random) (job: Job * Skill list) (parseDuration: int) =
            let rot = Rotation.empty (fst job)
            let state = fromRotation rot
            let rec genEpisode (rot: Rotation) (state: State) (finished: bool) (ep: (State * int * float) list) =
                if finished then
                    ep
                else
                    let probs = behavior state 
                    let action = 
                        let rand = random.Next(0, 1000000)
                        probs
                        |> List.indexed
                        |> List.fold (fun (prob, probs) (i, p) ->
                            let ap = p * 1000000.
                            let probU = prob + ap
                            probU, ((prob, probU, i) :: probs)
                        ) (0., [])
                        |> snd
                        |> List.skipWhile (fun (p, _pU, _) ->
                            int p > rand
                        )
                        |> List.head
                        |> fun (_, _, a) -> a
                    let nextState, reward, finished, nextRotation =
                        let skill = 
                            (snd job)
                            |> List.item action
                        let nextRot = Rotation.add skill rot
                        let nextState = fromRotation nextRot
                        let len, newLen, job, newJob =
                            match rot, nextRot with
                            | Rotation (oldT, oldJ), Rotation (newT, newJ) ->
                                List.length oldT, List.length newT, oldJ, newJ
                        let dmg = ToDamage nextRot false
                        //let reward = float <| (float dmg / float newLen) - (float dmg / 1800.)
                        //let reward = float dmg
                        let reward = float (newLen * dmg) / 1800.
                        let reward =
                            let skillres =
                                match skill.CostType with
                                | Free -> false
                                | TP d -> job.TP - d < 0
                                | MP d -> job.MP - d < 0
                            if newLen = len || skillres then 0. else reward
                        //printfn "%s,\t %f" skill.Name reward
                        let finished =
                            let time, job =
                                match nextRot with
                                | Rotation (ticks, j) -> List.length ticks > parseDuration, j
                            let skillres =
                                match skill.CostType with
                                | Free -> false
                                | TP d -> job.TP - d < 0
                                | MP d -> job.MP - d < 0
                            let lenDiff = 
                                newLen = len
                            time || skillres || lenDiff
                        nextState, reward, finished, nextRot
                    let episode = List.append ep [(state, action, reward)]
                    genEpisode nextRotation nextState finished episode
            genEpisode rot state false []
            
        let mcControlImportanceSampling (job: Job * Skill list) (parseDuration: int) (env: State) (episodes: int) (behavior: State -> float list) (discountFactor: float) (seed: int option) =
            let Q : Map<State, float list> = Map.empty
            let C : Map<State, float list> = Map.empty
            let nA = job |> snd |> List.length
            let random =
                match seed with
                | None -> System.Random()
                | Some s -> System.Random s

            let threads = 10
                
            let Q, _ =
                Array.Parallel.init threads (fun _ ->
                    Array.init (episodes / threads) (fun n ->
                        if n % 1000 = 0 then printf "."
                        genEpisode behavior random job parseDuration
                    )
                )
                |> Array.concat
                |> List.ofArray
                |> List.indexed
                |> List.fold (fun s (epID, episode) ->
                    //if epID % 10 = 0 then printfn "%d" epID
                    let Q, C = s
                
                    let G = 0.
                    let W = 1.

                    let G, C, Q, W =
                        (G, C, Q, W)
                        |> Seq.foldBack (fun v s ->
                            let state, action, reward = v
                            let G, (C: Map<State, float list>), (Q: Map<State, float list>), W = s
                            let G = discountFactor * G + reward
                            let newCVal = 
                                match C.TryFind state with
                                | Some s -> 
                                    s
                                    |> List.indexed
                                    |> List.map (fun (i, p) ->
                                        if i = action then p + W else p
                                    )
                                | None -> 
                                    List.init nA (fun _ -> 0.)
                                    |> List.indexed
                                    |> List.map (fun (i, p) ->
                                        if i = action then p + W else p
                                    )
                            let C = C.Add (state, newCVal)
                            let cVal =
                                match C.TryFind state with
                                | Some s -> 
                                    s |> List.item action
                                | None -> 0.
                            let newQVal =
                                match Q.TryFind state with
                                | Some s -> 
                                    s
                                    |> List.indexed
                                    |> List.map (fun (i, p) ->
                                        if i = action then p + (W / cVal) * (G - p) else p
                                    )
                                | None -> 
                                    List.init nA (fun _ -> 0.)
                                    |> List.indexed
                                    |> List.map (fun (i, p) ->
                                        if i = action then (W / cVal) * G else 0.
                                    )
                            let Q = Q.Add (state, newQVal)
                            let denom = 
                                behavior state
                                |> List.item action
                            let W = W * 1./denom
                            G, C, Q, W
                        ) episode
                    Q, C
                ) (Q, C)
            Q, createGreedyPolicy Q nA

        let runPolicy (job: Job * Skill list) (behavior: State -> float list) (parseDuration: int) (seed: int option) =
            let startT = System.DateTime.Now
            let rotation = Rotation.empty (fst job)
            let random =
                match seed with
                | None -> System.Random()
                | Some s -> System.Random s
                
            let episode =
                Array.init 1000 (fun _ -> genEpisode behavior random job parseDuration)
                |> Array.maxBy (fun ep ->
                    ep
                    |> List.map (fun (state, i, rew) -> List.item i (snd job), state, rew)
                    |> List.fold (fun s v ->
                        let skill, state, rew = v
                        //printfn "%s\t%f\t%A" skill.Name rew state
                        Rotation.add skill s
                    ) rotation
                    |> fun rot -> (ToDamage rot false)
                )
            let endT = System.DateTime.Now

            printfn "\nOptimal rotation: "
            let rot =
                episode
                |> List.map (fun (state, i, rew) -> List.item i (snd job), state, rew)
                |> List.fold (fun s v ->
                    let skill, state, rew = v
                    //let _, _, gcd, _, _, _, _ = state
                    printfn "%s\t%f\t%A" skill.Name rew state
                    Rotation.add skill s
                ) rotation
            printfn "DPS:\t%d\tGeneration Time: %A " (ToDPS rot true) (endT - startT)

