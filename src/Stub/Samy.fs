namespace SamyTheSalmon

open Aardvark.Application
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph

module Samy =
        
    // updates samys movement
    let updateMovement (input : Input.State) (p : V3d) (np : V3d) (dt : double) (s : State) =

        let view = s.camera.view |> Mod.force
        let mutable sp = s.samy.speed |> Mod.force
        let mutable v = s.samy.velocity |> Mod.force
        let mutable dash = s.samy.dash |> Mod.force

        // update dash timer
        dash <- (dash - dt) |> max 0.0

        // when samy leaves the water the movement speed (direction) is transformed into
        // velocity, so the player has no control over where samy flies
        if p.Z < 0.0 && np.Z >= 0.0 then

            v <- v + view.Forward * sp.Y + view.Right * sp.X
            sp <- V2d.Zero

        // when samy hits the water surface, reduce the velocity due to the impact
        if p.Z >= 0.0 && np.Z < 0.0 then

            let l = v.Length * 0.35
            v <- v.Normalized * l

        // under water
        if p.Z < 0.0 then

            // accelerate
            // make sure not to modify speed if already beyond limit (due to dash + clamp)
            if input.w && sp.Y <= Config.Samy.maxSpeed.Y  then
                sp.Y <- (sp.Y + 2.0 * dt) |> min Config.Samy.maxSpeed.Y

            // brake
            if input.s then
                sp.Y <- (sp.Y - 1.0 * dt) |> max 0.0

            // strafe
            if input.a then
                sp.X <- (sp.X - 2.0 * dt) |> max -Config.Samy.maxSpeed.X

            if input.d then
                sp.X <- (sp.X + 2.0 * dt) |> min Config.Samy.maxSpeed.X

            // dash
            if input.space && dash = 0.0 then
                sp.Y <- Config.Samy.dashSpeed
                dash <- Config.Samy.dashCooldown

            // water drag
            v <- let l = (v.Length - Config.Samy.dragWater * dt) |> max 0.0
                 v.Normalized * l

            sp.X <- match sign sp.X with
                        | 1  -> (sp.X - Config.Samy.dragWater * dt) |> max 0.0
                        | -1 -> (sp.X + Config.Samy.dragWater * dt) |> min 0.0
                        | _  -> 0.0
           
            sp.Y <- (sp.Y - Config.Samy.dragWater * dt) |> max 0.0

        else
            // gravity
            v <- v - V3d.OOI * Config.Samy.gravity * dt

            // air drag
            v <- let l = (v.Length - Config.Samy.dragAir * dt) |> max 0.0
                 v.Normalized * l


        // update mods
        s.samy.dash <== dash
        s.samy.velocity <== v
        s.samy.speed <== sp

        s

    // updates samys position
    let updatePosition (pos : V3d) (s : State) =
        if (s.samy.position |> Mod.force) <> pos then
            s.samy.position <== pos
        s
    
    // updates samy & camera according to controls
    let update (input : Input.State) (dt : double) (s : State) =

        // calculate new position
        let p = s.samy.position |> Mod.force
        let view = s.camera.view |> Mod.force
        let sp = s.samy.speed |> Mod.force
        let v = s.samy.velocity |> Mod.force
        let mutable newPos = p + (view.Forward * sp.Y + view.Right * sp.X + v) * dt 

        // calculate new direction
        let delta = input.mouseDelta
        let trafo =
            M44d.Rotation(view.Right, float delta.Y * -0.005) *
            M44d.Rotation(view.Sky, float delta.X * -0.005)
        let mutable newDir = trafo.TransformDir view.Forward |> Vec.normalize

        // very simple restriction
        newDir.Y <- min newDir.Y -0.1

        // check if new pos is valid
        newPos <- s |> CollisionDetection.riverbed newPos dt

        // checks for obstacle & goody collisions
        let checkObstacleCollision (o : Obstacle) = 
            newPos <- CollisionDetection.obstacles newPos s o dt
        let checkGoodyCollision (g : Goodie) = CollisionDetection.goodies newPos s g
        s.obstacles.instances |> Mod.force |> List.iter checkObstacleCollision
        s.goodies.instances |> Mod.force |> List.iter checkGoodyCollision

        // updates samy & camera
        let updatedState =
            s |> updatePosition newPos
              |> updateMovement input p newPos dt
              |> Camera.updateLocation newPos 
              |> Camera.updateDirection newDir

        updatedState