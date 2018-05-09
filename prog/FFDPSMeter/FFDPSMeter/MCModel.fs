namespace FFDPSMeter

    //module MCModel =
    //    open Model
    //    open Calculations

    //    open System.Collections.Generic

    //    type State = Tick
    //    type Action = Skill

    //    let MC (job: Job) (skillSet: Skill list) (time: int) (episodes: int) (seed: int option) =
    //        printf "-"
    //        let random = 
    //            match seed with
    //            | Some s -> System.Random s
    //            | None -> System.Random ()
    //        let s0 = Rotation.empty job
    //        let s = 
    //            match s0 with
    //            | Rotation (state, _) -> List.head state
    //        let b =
    //            let p = 1. / float skillSet.Length
    //            let ss =
    //                skillSet
    //                |> List.map (fun s ->
    //                    s, p
    //                )
    //            dict [s, ss]
    //        let pi = 
    //            let p = 1. / float skillSet.Length
    //            let ss =
    //                skillSet
    //                |> List.map (fun s ->
    //                    s, p
    //                )
    //            dict [s, ss]
    //        printf "-"
    //        let getAction (s: State) (pi: IDictionary<Tick, (Skill * float) list>) =
    //            let rand = random.Next(0, 1000)
    //            let skillset =
    //                match pi.TryGetValue s with
    //                | true, value -> value
    //                | false, _ ->
    //                    let p = 1. / float skillSet.Length
    //                    let ss =
    //                        skillSet
    //                        |> List.map (fun s ->
    //                            s, p
    //                        )
    //                    pi.Add (s, ss)
    //                    ss
    //            skillset
    //            |> List.fold (fun (prob, (ss: (float * float * Action) list)) (a, p) ->
    //                let ap = p * 1000.
    //                let probU = prob + ap
    //                probU, ((prob, probU, a) :: ss)
    //            ) (0., [])
    //            |> snd
    //            |> List.find (fun (p, pU, _) ->
    //                int p < rand && rand < int pU
    //            )
    //            |> fun (_, _, a) -> a

    //        Seq.init episodes (fun n -> n + 1)
    //        |> Seq.iter (fun n ->
    //            let rec addToRotation (r: Rotation) =
    //                printf "-"
    //                match r with
    //                | Rotation (ticks, _) ->
    //                    if ticks.Length > time then
    //                        r
    //                    else
    //                        let last = List.last ticks
    //                        addToRotation (Rotation.add (getAction last b) r)
    //            let rot = addToRotation (Rotation.empty job)
    //            match rot with
    //            | Rotation (rotation, job) ->
    //                rotation
    //                |> List.indexed
    //                |> List.iter (fun (i, s) ->
    //                    let combo =
    //                        match s.ActiveCombo with
    //                        | None -> "None"
    //                        | Some (c, _) -> sprintf "%s to %s" c.Name c.Target
    //                    match s.ActiveSkill with
    //                    | None -> ()
    //                    | Some s -> printf "%d:\tSkill: %20s\tPotency: %d\tCombo: %s\n" i s.Name s.Potency combo
    //                )
    //                printfn "MP: %d, TP: %d" job.MP job.TP
    //            printfn "DPS: %d" (ToDPS rot false)
    //        )

    module MCModel2 =

        open Model
        open Calculations

        type State = 
            (int list) * (int list) * (int list) * (string list) * int * int

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
                    match last.ActiveCombo with
                    | None -> []
                    | Some combos -> (fst combos) |> List.map (fun c -> c.Target) |> List.sort
                (buffs, ogcd, dots, combo, j.TP, j.MP)
        
        //type State = int list

        //let fromRotation (r: Rotation) : State =
        //    match r with
        //    | Rotation (ticks, _) ->
        //        ticks
        //        |> List.choose (fun t ->
        //            match t.ActiveSkill with
        //            | None -> None
        //            | Some s -> Some s.ID
        //        )

        let toKey (s: State) : int =
            let buffs, ogcd, _, _, _, _ = s
            buffs.Length + ogcd.Length

        let createRandomPolicy (n: int) =
            let ones = List.init n (fun _ -> 1. / float n)
            fun (s: State) -> ones

        let createEpsilonGreedyPolicy (Q: Map<int, Map<State, float list>>) (nA: int) (epsilon: float) =
            let policy (s: State) =
                let len = (toKey s)
                match Q.TryFind len with
                | Some maps -> 
                    match maps.TryFind s with
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
                | None -> 
                    List.init nA (fun _ -> if nA > 1 then 1. / float nA else 1.)
            policy

        let createGreedyPolicy (Q: Map<int, Map<State, float list>>) (n: int) =
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
                        let skillres =
                            match skill.CostType with
                            | Free -> false
                            | TP d -> job.TP - d < 0
                            | MP d -> job.MP - d < 0
                        let oldDmg = ToDamage rot false
                        let dmg = ToDamage nextRot false
                        //let reward = float <| (float dmg / float newLen) - (float dmg / 1800.)
                        let reward = float (dmg - oldDmg) / float (newLen - len)
                        //let reward = float newLen
                        //let reward = float <| dmg / newLen
                        //let reward = float (newLen * dmg) / parseDuration
                        //let reward = if newLen > parseDuration then 1. else 0.
                        //let reward = if newLen > len then 1. else 0.
                        //let reward = 
                        //    let len = if newLen > parseDuration then 1. else 0.
                        //    let combo = 
                        //        match rot with
                        //        | Rotation (ticks, _) -> 
                        //            let last = ticks |> List.last
                        //            match last.ActiveCombo with
                        //            | None -> 1.
                        //            | Some (c, _) -> 
                        //                if c.Target = skill.Name then 2. else 0.5
                        //    float newLen
                        //let reward = 10. * float (newLen - len) + 0.1 * float (dmg - oldDmg)
                        let reward =
                            if newLen = len then 
                                -50. 
                            elif newLen >= parseDuration then
                                100.
                            else 
                                reward
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
                            time || lenDiff
                        nextState, reward, finished, nextRot
                    let episode = List.append ep [(state, action, reward)]
                    genEpisode nextRotation nextState finished episode
            genEpisode rot state false []
            
        let mcControlImportanceSampling (job: Job * Skill list) 
                                        (parseDuration: int) 
                                        (episodes: int) 
                                        (behavior: State -> float list) 
                                        (Q : Map<int, Map<State, float list>>) 
                                        (C : Map<int, Map<State, float list>>) 
                                        (discountFactor: float) 
                                        (seed: int option)
                                        (threads: int) =
            let nA = job |> snd |> List.length
            let random =
                match seed with
                | None -> System.Random()
                | Some s -> System.Random s

            //let Q, C = Map.empty, Map.empty
                
            let Q, C =
                Array.Parallel.init threads (fun _ ->
                    Array.init (episodes / threads) (fun n ->
                        let spacing = 
                            if episodes <= 100 then
                                1
                            else 
                                1000
                        if n % spacing = 0 then printf "."
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
                            let G, (C: Map<int, Map<State, float list>>), (Q: Map<int, Map<State, float list>>), W = s
                            let G = discountFactor * G + reward
                            let len = toKey state
                            let C = 
                                let newCVal = 
                                    match C.TryFind len with
                                    | Some maps ->
                                        match maps.TryFind state with
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
                                    | None -> 
                                        List.init nA (fun _ -> 0.)
                                        |> List.indexed
                                        |> List.map (fun (i, p) ->
                                            if i = action then p + W else p
                                        )
                                match C.TryFind len with
                                | Some maps ->
                                    C.Add (len, maps.Add (state, newCVal))
                                | None -> 
                                    C.Add (len, Map [(state, newCVal)])
                            let cVal =
                                match C.TryFind len with
                                | Some maps -> 
                                    match maps.TryFind state with
                                    | Some s ->
                                        s |> List.item action
                                    | None -> 0.
                                | None -> 0.
                            let Q = 
                                let newQVal =
                                    match Q.TryFind len with
                                    | Some maps -> 
                                        match maps.TryFind state with
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
                                    | None -> 
                                        List.init nA (fun _ -> 0.)
                                        |> List.indexed
                                        |> List.map (fun (i, p) ->
                                            if i = action then (W / cVal) * G else 0.
                                        )
                                match Q.TryFind len with
                                | Some maps ->
                                    Q.Add (len, maps.Add (state, newQVal))
                                | None -> 
                                    Q.Add (len, Map [(state, newQVal)])
                            let denom = 
                                behavior state
                                |> List.item action
                            let W = W * 1./denom
                            G, C, Q, W
                        ) episode
                    Q, C
                ) (Q, C)
            Q, C, createGreedyPolicy Q nA

        open System.IO

        let printToFile (ep: (State * int * float) list) (job: Job * Skill list) (Q: Map<int, Map<State, float list>>) (folder : string) =
            let filename = System.DateTime.Now.ToString("yyyyMMdd-Hmmss") + ".log"
            let dir = Path.Combine (Directory.GetCurrentDirectory(), "logs", folder)
            Directory.CreateDirectory dir |> ignore
            let path = Path.Combine (dir, filename)
            use file = StreamWriter path
            let rot =
                ep
                |> List.map (fun (state, i, rew) -> (i, List.item i (snd job)), state, rew)
                |> List.fold (fun s v ->
                    let (i, skill), state, rew = v
                    //let _, _, gcd, _, _, _, _ = state
                    let len = toKey state
                    let value = 
                        match Q.TryFind len with
                        | Some maps ->
                            match maps.TryFind state with
                            | Some v -> sprintf "(%f, %A)"(List.item i v) (string v)
                            | None -> "None"
                        | None -> "None"
                    file.WriteLine (sprintf "%20s\t%f\t%A\t%A" skill.Name rew value state)
                    Rotation.add skill s
                ) (Rotation.empty (fst job))
            match rot with
            | Rotation (ticks, job) ->
                ticks
                |> List.indexed
                |> List.fold (fun dps (index, tick) ->
                    let buffs =
                        tick.ActiveBuffs
                        |> List.fold (fun (s: Skill -> Skill) (v, _) ->
                            match v.Effect with
                            | BuffType.Skill b -> s >> b
                            | _ -> id
                        ) id
                    let debuffs =
                        tick.ActiveDebuffs
                        |> List.fold (fun (s: Skill -> Skill) (v, _) ->
                            match v.Effect with
                            | BuffType.Skill b -> s >> b
                            | _ -> id
                        ) id

                    let autoattack =
                        if index % 30 = 0 then
                            Some <| (buffs >> debuffs) job.AutoAttack
                        else
                            None
                    let activeskill =
                        match tick.ActiveSkill with
                        | None -> None
                        | Some s -> Some s

                    let dotPotency =
                        tick.ActiveDoTs
                        |> List.fold (fun s (v, (start, _)) ->
                            if index > start && (index - start) % 30 = 0 then
                                s + v.Potency
                            else
                                0
                        ) 0
                    let aaPotency = 
                        match autoattack with
                        | None -> 0
                        | Some autoattack -> autoattack.Potency
                    let activePotency =
                        match activeskill with
                        | None -> 0
                        | Some active -> active.Potency
                    if dotPotency + aaPotency + activePotency > 0 then
                        let buffsPrint =
                            tick.ActiveBuffs
                            |> List.map (fun (b, i) -> sprintf "(%s, %d, %d)" b.Name b.Stacks i)
                        let relativeDPS =
                            if index < 10 then 
                                0
                            else
                                (dps / (index / 10))
                        file.WriteLine (sprintf "DPS (tick %d)\tAutoAttack: %d\tDoT: %d\tSkill: %d\tDamage: %d\tDPS: %d" index aaPotency dotPotency activePotency dps relativeDPS)
                    dps + dotPotency + aaPotency + activePotency
                ) 0
                |> ignore
            file.Flush ()
            file.Close ()

        let policyToSkillList (job: Job * Skill list) (behavior: State -> float list) (parseDuration: int) (seed: int option) =
            let rotation = Rotation.empty (fst job)
            let random =
                match seed with
                | None -> System.Random()
                | Some s -> System.Random s
            let ep = genEpisode behavior random job parseDuration
                //Array.init 100 (fun _ -> genEpisode behavior random job parseDuration)
                //|> Array.maxBy (fun ep ->
                //    ep
                //    |> List.map (fun (state, i, rew) -> List.item i (snd job), state, rew)
                //    |> List.fold (fun s v ->
                //        let skill, _, _ = v
                //        Rotation.add skill s
                //    ) rotation
                //    |> fun rot -> (ToDamage rot false)
                //)
            ep
            |> List.map (fun (_, i, _) ->
                (snd job).Item i
            )

        let runPolicy (job: Job * Skill list) (behavior: State -> float list) (parseDuration: int) (Q: Map<int, Map<State, float list>>) (seed: int option) (folder: string) =
            let startT = System.DateTime.Now
            let rotation = Rotation.empty (fst job)
            let random =
                match seed with
                | None -> System.Random()
                | Some s -> System.Random s
                
            let episode = //genEpisode behavior random job parseDuration
                Array.init 1000 (fun _ -> genEpisode behavior random job parseDuration)
                |> Array.maxBy (fun ep ->
                    ep
                    //|> List.fold (fun s (_, _, rew) ->
                    //    s + rew
                    //) 0.
                    |> List.map (fun (state, i, rew) -> List.item i (snd job), state, rew)
                    |> List.fold (fun s v ->
                        let skill, state, rew = v
                        Rotation.add skill s
                    ) rotation
                    |> fun rot -> (ToDamage rot false)// / parseDuration
                )
            let endT = System.DateTime.Now

            printToFile episode job Q folder
            printfn "\nOptimal rotation: "
            let rot =
                episode
                |> List.map (fun (state, i, rew) -> (i, List.item i (snd job)), state, rew)
                |> List.fold (fun s v ->
                    let (i, skill), state, rew = v
                    //let _, _, gcd, _, _, _, _ = state
                    let len = toKey state
                    let value = 
                        match Q.TryFind len with
                        | Some maps ->
                            match maps.TryFind state with
                            | Some v -> (List.item i v), string v //sprintf "%A" v
                            | None -> 0., "None"
                        | None -> 0., "None"
                    printfn "%20s\t%f\t%A\t%A" skill.Name rew value state
                    Rotation.add skill s
                ) rotation
            printfn "DPS:\t%d\tGeneration Time: %A " (ToDPS rot true) (endT - startT)

