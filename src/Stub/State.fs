namespace SamyTheSalmon

open System.IO

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.Rendering.GL
open Aardvark.SceneGraph

module State =

    // initial game state
    let initial = { 
        status = Initializing;
        score = Mod.init 0;
        samy = 
            { 
                position = Mod.init Game.Constants.spawn
                speed = Mod.init V2d.Zero
                velocity = Mod.init V3d.Zero
                dash = Mod.init 0.0
                health = Mod.init 100.0 
            };
        camera =
            { 
                view = CameraView.Look(Game.Constants.spawn, Game.Constants.direction) |> Mod.init;
                frustum = Unchecked.defaultof<_>
            };
        water = { offset = Mod.init 0.0 };
        lights = Lighting.zero |> Array.create Config.Lighting.maxLights |> Mod.init;
        obstacles =
            { 
                timer = 0.0;
                instances = Mod.init [] 
            };
        goodies =
            {
                instances = Mod.init []
            }
        fireflies =
            {
                instances = Mod.init []
            }
        playername = File.readAllText @"..\resources\playername.txt" |> Mod.init
    }

    // process the game state
    // this only handles the state transitions and should not contain any state specific logic
    let interact (win : GameWindow) (r : Runtime) (s : State) (input : Input.State) (dt : double) =

        match s.status with

            // initializing (can only happen once obviously) 
            | Initializing ->
                StartScreen.init r win s

            // start screen
            | ShowingInstructions ->
                StartScreen.update input s

            // initializing the game
            | InitializingGame ->
                Game.init r win s

            // running the game
            | Running ->
                if Game.hasPlayerWon s then
                    FinishScreen.playerWon r win s
                else if Game.hasPlayerLost s then
                    FinishScreen.playerLost r win s
                else
                    Game.update input dt s win

            // ending screen
            | EndOfGame -> 
                if input.enter then
                    win.Close()
                s