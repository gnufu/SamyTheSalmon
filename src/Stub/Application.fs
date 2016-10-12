namespace SamyTheSalmon

open System
open System.IO

open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.Base
open Aardvark.Base.Ag
open Aardvark.Base.Incremental
open Aardvark.Rendering.NanoVg
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics


module Application =

    let run() =

        // initialize modules
        Aardvark.Init()
        Assimp.init()

        // setup window
        let win = Config.app.CreateGameWindow()
        let runtime = Config.app.Runtime

        win.Title <- "Samy the Salmon"
        win.WindowState <- match Config.Application.fullscreen with
                                | true -> OpenTK.WindowState.Fullscreen
                                | _ -> OpenTK.WindowState.Normal
        win.CursorVisible <- false

        // keep track of game state
        let mutable state = State.initial

        // ESC closes application
        win.Keyboard.KeyDown(Keys.Escape).Values.Subscribe(fun _ ->
            win.Close() |> ignore
        ) |> ignore

        // feedback messages 
        KeyMappings.displayFeedbackMsg win runtime

        let mutable oldState = None

        // per frame callback
        let updateFrame (args : OpenTK.FrameEventArgs) =

            // grab input & process
            let newState = Input.grab win oldState
            oldState <- Some newState
            state <- State.interact win runtime state newState (args.Time |> min Config.Application.deltaCap)

        // run
        win.UpdateFrame.Add(updateFrame)
        win.Run()