namespace App

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.ObjectModel
open System.Collections.Generic
open System.ComponentModel
open System.Windows


open FSharp.Charting
open FSharp.Charting.ChartTypes
open ViewModule
open ViewModule.FSharp

open FFDPSMeter.MCModel2
open FFDPSMeter.Model

type MainViewModel () as this =
    inherit ViewModelBase()

    let drgJob, drgSkills, drg =
        let skills =
            FFDPSMeter.Skills.Dragoon.Skillset
            |> List.mapi (fun i s -> SkillView (s, i))
        FFDPSMeter.Skills.Dragoon.Job, FFDPSMeter.Skills.Dragoon.Skillset, skills
    let pldJob, pldSkills, pld =
        let skills =
            FFDPSMeter.Skills.Paladin.Skillset
            |> List.mapi (fun i s -> SkillView (s, i))
        FFDPSMeter.Skills.Paladin.Job, FFDPSMeter.Skills.Paladin.Skillset, skills
    let dragoonSkills = ObservableCollection<SkillView> (drg)
    let paladinSkills = ObservableCollection<SkillView> (pld)
    let skills = ObservableCollection<SkillView>()
    let result = ObservableCollection<SkillView>()
    let liveResultChart = ObservableCollection<KeyValuePair<int, int>> ()
    let resultDPSChart = ObservableCollection<KeyValuePair<int, int>> ()
    let generationsChart = ObservableCollection<KeyValuePair<int, int>> ()
    let generationDurationsChart = ObservableCollection<KeyValuePair<int, int>> ()

    let mutable bgWorkerSet = false
    let mutable Q = Map.empty
    let mutable C = Map.empty
    let mutable policy = fun _ -> []
    let mutable job = drgJob
    let mutable skillset = drgSkills
    
    let episodes = this.Factory.Backing(<@ this.Episodes @>, 100)
    let generations = this.Factory.Backing(<@ this.Generations @>, 0)
    let toGenerate = this.Factory.Backing(<@ this.ToGenerate @>, 1)
    let duration = this.Factory.Backing(<@ this.Duration @>, 1800)
    let threads = this.Factory.Backing(<@ this.Threads @>, 5)
    let seed = this.Factory.Backing(<@ this.Seed @>, 0)
    let discountFactor = this.Factory.Backing(<@ this.DiscountFactor @>, 0.9)
    let working = this.Factory.Backing(<@ this.Working @>, false)
    let enabled = this.Factory.Backing(<@ this.Enabled @>, true)
    let started = this.Factory.Backing(<@ this.Started @>, true)

    let statusText = this.Factory.Backing(<@ this.StatusText @>, "Add something to the skillset")
    let totalDamageText = this.Factory.Backing(<@ this.TotalDamage @>, "0")
    let totalDurationText = this.Factory.Backing(<@ this.TotalDuration @>, "0")
    
    let mutable bgWorker = new BackgroundWorker ()

    let clear () =
        Q <- Map.empty
        C <- Map.empty
        policy <- fun _ -> []
        bgWorkerSet <- false
        generations.Value <- 0
        liveResultChart.Clear()
        generationsChart.Clear()
        generationDurationsChart.Clear()
        resultDPSChart.Clear()
        result.Clear()
        skills.Clear()
        started.Value <- true

    let newSkillset =
        this.Factory.CommandSync(fun _ -> 
            statusText.Value <- "Skillset cleared"
            clear ()
        )

    let addSkill = 
        this.Factory.CommandSyncParam(fun (s: int) -> 
            MessageBox.Show (string s) |> ignore
            statusText.Value <- string s
        )

    let addDragoon = 
        this.Factory.CommandSync(fun _ -> 
            statusText.Value <- "Dragoon added to the skillset"
            job <- drgJob
            skillset <- drgSkills
            clear ()
            skills.Clear()
            dragoonSkills |> Seq.iter (fun s -> skills.Add s)
        )
        
    let addPaladin = 
        this.Factory.CommandSync(fun _ -> 
            statusText.Value <- "Paladin added to the skillset"
            job <- pldJob
            skillset <- pldSkills
            clear ()
            skills.Clear()
            paladinSkills |> Seq.iter (fun s -> skills.Add s)
        )

    let doML =
        DoWorkEventHandler (fun o e -> 
            working.Value <- true
            enabled.Value <- false
            Seq.init toGenerate.Value id
            |> Seq.iter (fun _ ->
                if skills.Count > 0 && not <| bgWorker.CancellationPending then
                    let skillset = skills |> Seq.map (fun s -> s.ToMLModel skillset) |> List.ofSeq
                    if generations.Value = 0 then
                        policy <- createRandomPolicy skillset.Length
                    let newQ, newC, greedyPolicy = mcControlImportanceSampling (job, skillset) (duration.Value) (episodes.Value) policy Q C (discountFactor.Value) (if seed.Value = 0 then None else Some seed.Value) (threads.Value)
                    Q <- newQ
                    C <- newC
                    policy <- createEpsilonGreedyPolicy Q (skillset.Length) (1. / (5. * System.Math.Log (float generations.Value + 5.)))
                    let skillist = policyToSkillList (job, skillset) greedyPolicy duration.Value (if seed.Value = 0 then None else Some seed.Value)
                    let res = 
                        skillist
                        |> List.mapi (fun i s -> SkillView (s, i))
                    let rot = FFDPSMeter.Calculations.ToRotation skillist job
                    let dmg = FFDPSMeter.Calculations.ToDamage rot false
                    let time = 
                        match rot with | Rotation (ticks, _) -> ticks.Length
                    let resChart, dpsChart, _ =
                        res
                        |> Seq.map (fun s -> s.ToMLModel skillset)
                        |> Seq.fold (fun (resChart, dpsChart, rot) value ->
                            let rot = Rotation.add value rot
                            let time =
                                match rot with
                                | Rotation (ticks, _) -> ticks.Length
                            let dmg = FFDPSMeter.Calculations.ToDamage rot false
                            (time, dmg) :: resChart, (time, if time > 0 then dmg / (time / 10) else 0) :: dpsChart, rot
                        ) ([],[],Rotation.empty job)
                    bgWorker.ReportProgress(0, (resChart, dpsChart, time, res, dmg) :> obj)
            )
        )
        
    let reportProgress =
        ProgressChangedEventHandler(fun o e -> 
            let resChart, dpsChart, time, res, dmg = e.UserState :?> ((int * int) list * (int * int) list * int * SkillView list * int)
            generations.Value <- generations.Value + 1
            totalDamageText.Value <- string dmg
            totalDurationText.Value <- string time
            result.Clear()
            res |> Seq.iter (fun s -> result.Add s)
            resultDPSChart.Clear()
            liveResultChart.Clear()
            resChart
            |> List.iter (fun (k, v) -> liveResultChart.Add(KeyValuePair<int, int> (k, v)))
            dpsChart
            |> List.iter (fun (k, v) -> resultDPSChart.Add(KeyValuePair<int, int> (k, v)))
            generationsChart.Add(KeyValuePair<int, int> (generations.Value, dmg))
            generationDurationsChart.Add(KeyValuePair<int, int> (generations.Value, time))
        )

    let workerCompleted =
        RunWorkerCompletedEventHandler(fun _ _ -> 
            working.Value <- false
            enabled.Value <- true
        )

    let startML =
        this.Factory.CommandSync(fun _ ->
            let genNumVal, epVal, durVal, threadVal, discVal =
                toGenerate.Value > 0, episodes.Value > 0, duration.Value > 0, threads.Value > 0, not (discountFactor.Value < 0. || discountFactor.Value > 1.)
            if not genNumVal then
                MessageBox.Show "Generation # must be positive!" |> ignore
            if not epVal then
                MessageBox.Show "Episodes must be positive!" |> ignore
            if not durVal then
                MessageBox.Show "Duration must be positive!" |> ignore
            if not threadVal then
                MessageBox.Show "Threads must be positive!" |> ignore
            if not discVal then
                MessageBox.Show "Discount factor must be between 0 and 1!" |> ignore
            if genNumVal && epVal && durVal && threadVal && discVal then
                if not bgWorkerSet && skills.Count > 0 then
                    bgWorker.DoWork.RemoveHandler doML
                    bgWorker.ProgressChanged.RemoveHandler reportProgress
                    bgWorker.RunWorkerCompleted.RemoveHandler workerCompleted
                    started.Value <- false
                    bgWorkerSet <- true
                    bgWorker.WorkerReportsProgress <- true
                    bgWorker.DoWork.AddHandler doML
                    bgWorker.ProgressChanged.AddHandler reportProgress
                    bgWorker.RunWorkerCompleted.AddHandler workerCompleted
                    bgWorker.WorkerSupportsCancellation <- true
                bgWorker.RunWorkerAsync ()
        )

    let cancelGeneration =
        this.Factory.CommandSync(fun _ ->
            bgWorker.CancelAsync ()
        )

    member __.ResultChart = liveResultChart
    member __.ResultDPSChart = resultDPSChart
    member __.GenerationsChart = generationsChart
    member __.GenerationDurationsChart = generationDurationsChart
    member __.ShowDragoon = dragoonSkills
    member __.ShowPaladin = paladinSkills
    member __.AddSkill = addSkill
    member __.Skills = skills
    member __.Result = result
    member __.MLButton = startML
    member __.AddDragoon = addDragoon
    member __.AddPaladin = addPaladin
    member __.NewSkillset = newSkillset
    member __.CancelGeneration = cancelGeneration
    member __.DiscountFactor with get () = discountFactor.Value
                                and set value = discountFactor.Value <- value
    member __.Started with get () = started.Value
                            and set value = started.Value <- value
    member __.Working with get () = working.Value
                            and set value = working.Value <- value
    member __.Enabled with get () = enabled.Value
                            and set value = enabled.Value <- value
    member __.Threads with get () = threads.Value
                            and set value = threads.Value <- value
    member __.Seed with get () = seed.Value
                            and set value = seed.Value <- value
    member __.Duration with get () = duration.Value
                            and set value = duration.Value <- value
    member __.Episodes with get () = episodes.Value
                            and set value = episodes.Value <- value
    member __.ToGenerate with get () = toGenerate.Value
                            and set value = toGenerate.Value <- value
    member __.Generations with get () = generations.Value
                            and set value = generations.Value <- value
    member __.TotalDamage with get () = totalDamageText.Value
                            and set value = totalDamageText.Value <- value
    member __.TotalDuration with get () = totalDurationText.Value
                            and set value = totalDurationText.Value <- value
    member __.StatusText with get () = statusText.Value
                            and set value = statusText.Value <- value
