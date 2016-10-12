namespace SamyTheSalmon

open Aardvark.Application
open Aardvark.Base
open Aardvark.Base.Incremental

module KeyMappings =

    (*
    F1 - Help (if available)
    F2 - Frame Time on/off <-- DONE (statistics overlay)
    F3 - Wire Frame on/off <-- DONE 
    F4 - Textur-Sampling-Quality: Nearest Neighbor/Bilinear <-- DONE 
    F5 - Mip Mapping-Quality: Off/Nearest Neighbor/Linear <-- DONE 
    F6 - -
    F7 - -
    F8 - Viewfrustum-Culling on/off <-- DONE
    F9 - Transparency on/off <-- DONE 
    *)

    // timer for displaying message 
    let private msgTimer eventHandler ms =
        let timer = new System.Timers.Timer(1000.0)
        timer.AutoReset <- true
        timer.Elapsed.Add eventHandler
        async {
            timer.Start()
            do! Async.Sleep ms
            Config.Application.feedbackMsg <== ""
            timer.Stop()
        } 
        
    let private stopDisplayingInMilliseconds ms =
        let basicHandler _ = printfn "displaying msg: %A" System.DateTime.Now
        msgTimer basicHandler ms |> Async.Start               

    // subscriptions for feedback messages
    let displayFeedbackMsg (win : GameWindow) (runtime : IRuntime) =

        // display statistics overlay on / off
        win.Keyboard.KeyDown(Keys.F2).Values.Subscribe(fun _ ->
            Config.Application.showOverlay <== not (Config.Application.showOverlay |> Mod.force)
            Config.Application.feedbackMsg <==
                match Config.Application.showOverlay |> Mod.force with
                | true -> "Turned statistics overlay on."
                | false -> "Turned statistics overlay off."
            stopDisplayingInMilliseconds 1500
        ) |> ignore

        // texture samling quality: nearest neighbor / bilinear
        win.Keyboard.KeyDown(Keys.F4).Values.Subscribe(fun _ ->
            let (quality, msg) = 
                match Config.Application.textureSamlingQuality |> Mod.force with
                | TextureSampling.Bilinear -> 
                    (TextureSampling.NearestNeigbor, "Switched Textur-Sampling-Quality to Nearest Neighbor.")
                | TextureSampling.NearestNeigbor -> 
                    (TextureSampling.Bilinear, "Switched Textur-Sampling-Quality to Bilinear.")
            Config.Application.textureSamlingQuality <== quality
            Config.Application.feedbackMsg <== msg
            stopDisplayingInMilliseconds 5000
        ) |> ignore

        // wire frame on / off
        win.Keyboard.KeyDown(Keys.F3).Values.Subscribe(fun _ ->
            Config.Application.wireFrame <== not (Config.Application.wireFrame |> Mod.force)
            Config.Application.feedbackMsg <==
                match Config.Application.wireFrame |> Mod.force with
                | true -> "Turned wire frame on."
                | false -> "Turned wire frame off."
            stopDisplayingInMilliseconds 1500
        ) |> ignore

        // mip mapping quality: off / nearest neighbor / linear
        win.Keyboard.KeyDown(Keys.F5).Values.Subscribe(fun _ ->
            let (quality, msg) =
                match Config.Application.mipMappingQuality |> Mod.force with
                | MipMapping.Off ->
                    (MipMapping.NearestNeighbor, "Switched Mip Mapping-Quality to Nearest Neighbor.")
                | MipMapping.NearestNeighbor ->
                    (MipMapping.Linear, "Switched Mip Mapping-Quality to Linear.")
                | MipMapping.Linear -> 
                    (MipMapping.Off, "Turned Mip Mapping off.")
            Config.Application.mipMappingQuality <== quality
            Config.Application.feedbackMsg <== msg
            stopDisplayingInMilliseconds 5000
        ) |> ignore

        // frustum culling on / off
        win.Keyboard.KeyDown(Keys.F8).Values.Subscribe(fun _ ->
            Config.Application.frustumCulling <== not (Config.Application.frustumCulling |> Mod.force)
            Config.Application.feedbackMsg <==
                match Config.Application.frustumCulling |> Mod.force with
                | true -> "Turned frustum culling on."
                | false -> "Turned frustum culling off."
            stopDisplayingInMilliseconds 1500
        ) |> ignore

        // transparency on / off
        win.Keyboard.KeyDown(Keys.F9).Values.Subscribe(fun _ ->
            Config.Application.transparency <== not (Config.Application.transparency |> Mod.force)
            Config.Application.feedbackMsg <==
                match Config.Application.transparency |> Mod.force with
                | true -> "Turned transparency on."
                | false -> "Turned transparency off."
            stopDisplayingInMilliseconds 1500
        ) |> ignore


