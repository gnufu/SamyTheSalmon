namespace SamyTheSalmon

open System.IO

open Aardvark.Application
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.GL
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics

module RenderTasks =
    open Aardvark.Base.Rendering

    let private compile (r : Runtime) (win : GameWindow) (sg : ISg) = 
        let config = { BackendConfiguration.Default with useDebugOutput = true }
        r.CompileRender(win.FramebufferSignature, config, sg)
        //r.CompileRender(win.FramebufferSignature, sg.RenderObjects())

    // render task for start screen
    let startScreen (r : Runtime) (win : GameWindow) (s : State) = 

        let screen = SceneGraphs.startSg |> compile r win
        let playername = Text.playername r s.playername win.Sizes |> compile r win
        win.RenderTask <- RenderTask.ofList [screen; playername]

    // render task for running game
    let gameRunning  (r : Runtime) (win : GameWindow) (s : State) =

        let world =  (World.sg win s) |> compile r win
        //let health = Text.health r s |> compile r win
        //let score = Text.score r s |> compile r win
        //let rt = RenderTask.ofList [world; health; score]
        let rt = world
        
        let overall = 
            adaptive {
                let! s = Config.Application.showOverlay
                let! m = Config.Application.feedbackMsg

                // check if we want to see the statistics overlay
                let withStats = 
                    match s with
                    | true -> rt //|> DefaultOverlays.withStatistics
                    | false -> rt

                // check if there is something in the message box to show
                let withMsg =
                    match m.IsEmptyOrNull() with
                    | true -> withStats
                    | false -> 
                        let msg = Text.feedbackMsg r win.Sizes |> compile r win
                        RenderTask.ofList [withStats; msg]
                return withMsg
            } |> RenderTask.ofMod
        
        win.RenderTask <- overall

    // render task for end screen
    let private endScreen (backgroundSg : ISg) (r : Runtime) (win : GameWindow) (s : State) (hs : string)  =      
        let screen = backgroundSg |> compile r win
        let highscore = Text.highscore r (hs |> Mod.init) win.Sizes |> compile r win
        win.RenderTask <- RenderTask.ofList [screen; highscore]

    // render task for finish screen
    let finishScreen (r : Runtime) (win : GameWindow) (s : State) (hs : string) =
        endScreen SceneGraphs.finishSg r win s hs
        
    // render task for player lost screen
    let playerLostScreen (r : Runtime) (win : GameWindow) (s : State) (hs : string) =
        endScreen SceneGraphs.playerLostSg r win s hs


    