namespace SamyTheSalmon

open Aardvark.Application
open Aardvark.Base
open Aardvark.Base.Incremental

module KeyMappings =
    open System

    (*
    F1 - -
    F2 - -
    F3 - Wire Frame on/off 
    F4 - Textur-Sampling-Quality: Nearest Neighbor/Bilinear
    F5 - Mip Mapping-Quality: Off/Nearest Neighbor/Linear 
    F6 - -
    F7 - -
    F8 - Viewfrustum-Culling on/off 
    F9 - Transparency on/off 
    *)

    let private printMsg (m : string) = 
        Console.ForegroundColor <- ConsoleColor.Cyan 
        Console.WriteLine(m)
        Console.ForegroundColor <- ConsoleColor.White
    
    // subscriptions for feedback messages
    let displayFeedbackMsg (win : GameWindow) (runtime : IRuntime) =
 
        // texture samling quality: nearest neighbor / bilinear
        win.Keyboard.KeyDown(Keys.F4).Values.Subscribe(fun _ ->
            let (quality, msg) = 
                match Config.Application.textureSamlingQuality |> Mod.force with
                | TextureSampling.Bilinear -> 
                    (TextureSampling.NearestNeigbor, "Switched Textur-Sampling-Quality to Nearest Neighbor.")
                | TextureSampling.NearestNeigbor -> 
                    (TextureSampling.Bilinear, "Switched Textur-Sampling-Quality to Bilinear.")
            Config.Application.textureSamlingQuality <== quality
            printMsg msg
        ) |> ignore

        // wire frame on / off
        win.Keyboard.KeyDown(Keys.F3).Values.Subscribe(fun _ ->
            Config.Application.wireFrame <== not (Config.Application.wireFrame |> Mod.force)
            let msg =
                match Config.Application.wireFrame |> Mod.force with
                | true -> "Turned wire frame on."
                | false -> "Turned wire frame off."
            printMsg msg
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
            printMsg msg
        ) |> ignore

        // frustum culling on / off
        win.Keyboard.KeyDown(Keys.F8).Values.Subscribe(fun _ ->
            Config.Application.frustumCulling <== not (Config.Application.frustumCulling |> Mod.force)
            let msg =
                match Config.Application.frustumCulling |> Mod.force with
                | true -> "Turned frustum culling on."
                | false -> "Turned frustum culling off."
            printMsg msg
        ) |> ignore

        // transparency on / off
        win.Keyboard.KeyDown(Keys.F9).Values.Subscribe(fun _ ->
            Config.Application.transparency <== not (Config.Application.transparency |> Mod.force)
            let msg =
                match Config.Application.transparency |> Mod.force with
                | true -> "Turned transparency on."
                | false -> "Turned transparency off."
            printMsg msg
        ) |> ignore


