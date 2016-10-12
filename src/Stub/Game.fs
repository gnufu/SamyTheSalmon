namespace SamyTheSalmon

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.SceneGraph
open Aardvark.Rendering.GL

// Main game logic
module Game =

    module Constants =

        let spawn = V3d(-0.2, 9.8, -0.2)

        let direction = -V3d.OIO

        let finish = -9.5

    // returns if player won
    let hasPlayerWon (s : State) =
        let p = s.samy.position |> Mod.force
        p.Y < Constants.finish

    // returns if player lost
    let hasPlayerLost (s : State) =
        let hp = s.samy.health |> Mod.force
        hp <= 0.0

    // initializes the game
    let init (r : Runtime) (win : GameWindow) (s : State) =

        // set lighting of the scene
        let l = s.lights |> Mod.force
        l.[0] <- Lighting.directional (V3d(-0.25, -0.7, 1.0)) (Config.World.sun * 0.075) Config.World.sun
        //l.[1] <- Lighting.spot (V3d(0.0, 0.0, -0.1)) (-V3d.OIO) 60.0 (V3d(0.1)) (C4d.Yellow * 0.002) C4d.Yellow
        s.lights <== l

        // build view frustum
        let frustum = win.Sizes |> Mod.map (fun s -> Frustum.perspective 90.0 0.01 1000.0 (float s.X / float s.Y))

        // new state
        let s =
            { s with status = Running
                     camera = { s.camera with frustum = frustum } }

        // prepare render task
        RenderTasks.gameRunning r win s

        // start playing sound
        Audio.playSound GameContent.Media.player

        // start stopwatch for calculation of player's score
        Config.Application.stopwatch.Start()

        s

    // process game logic
    let update (input : Input.State) (dt : double) (s : State) (win : GameWindow) =

        s |> Water.update dt
          |> Obstacles.update dt
          |> Samy.update input dt 
          |> Goodies.update dt
          |> Fireflies.update dt 