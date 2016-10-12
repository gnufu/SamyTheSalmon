namespace SamyTheSalmon

open System

open Aardvark.Application
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Base.Ag

// TODO: spawning interval should decrease as the game progresses, but how?
// TODO: speed should increase with time
module Obstacles =

    // random number generator
    let private rand = Random ()

    // creates random V3d for spin
    let private genSpin () =
        Array.init 3 (fun _ -> (rand.NextDouble() * 2.0 - 1.0) * Config.Obstacles.maxSpin) |> V3d

    // randomy create the spin of the boots
    let private bootsSpin_1 =  genSpin ()
    let private bootsSpin_2 =  genSpin ()

    // computes the bounding box of an obstacle
    let private bb (o : Obstacle) =

        let p = Path.toWorldSpace o.position o.path
        let transformedSg = o.sg
                            |> Sg.trafo o.rotation
                            |> Sg.trafo (p |> Mod.map Trafo3d.Translation)

        transformedSg?LocalBoundingBox() : IMod<Box3d>
        
    // spawns a new obstacle and adds it to the list if t = true
    let private spawn (t : bool) (playerPos : V3d) (inst : Obstacle list)  =

        let types = GameContent.Content.obstacleTypes

        if not t then
            inst
        else
            // select random path
            let pths = GameContent.Content.paths
            let pth = pths.[pths.Length |> rand.Next]

            // find spawning position relative to player position
            let p = 
                let p = pth |> Path.toPosition playerPos
                (p - Config.Obstacles.relativeOffsetSpawn) |> max 0.0

            // create new obstacle
            let obs = {
                    sg = types.[rand.Next types.Length];
                    position = Mod.init p;
                    rotation = Mod.init Trafo3d.Identity;
                    spin = genSpin ()
                    path = pth;
                    boundingbox = Unchecked.defaultof<_>
                }

            // append and add proper bounding box
            { obs with boundingbox = bb obs } :: inst

    // updates the obstacles, spawning new ones if needed and adjusting the spawning interval
    let update (dt : double) (s : State) =

        let obs = s.obstacles
        let playerPos = (s.camera.view |> Mod.force).Location

        // update timer
        let t = obs.timer - dt

        // update obstacle positions and rotations; remove obstacles that reached the end
        // spawn a new obstacle if timer reached zero
        let inst =

            let positionLimit (o : Obstacle) =
                let pp = o.path |> Path.toPosition playerPos
                (pp + Config.Obstacles.relativeOffsetLimit) |> min 1.0

            obs.instances
                |> Mod.force
                |> List.map (fun o -> 
                    o.position <=+ dt * Config.Obstacles.flowSpeedStart 
                    o.rotation <=* Trafo3d.Rotation(o.spin * dt)
                    o)
                |> List.filter (fun o -> (o.position |> Mod.force) < (positionLimit o))
                |> spawn (t < 0.0) playerPos

        // restart timer if below zero
        let t = if t < 0.0 then Config.Obstacles.intervalStart + t else t

        // also upate the spin trafos of the boot obstacles
        Config.Obstacles.bootsRotation_1 <=* Trafo3d.Rotation(bootsSpin_1 * dt)
        Config.Obstacles.bootsRotation_2 <=* Trafo3d.Rotation(bootsSpin_2 * dt)

        // return new state
        obs.instances <== inst
        { s with obstacles = { timer = t; instances = obs.instances } }


[<AutoOpen>]
module SgObstaclesExtensions =

    module Sg =

        // builds a set of sgs from a list of obstacles
        let obstacles (instances : IModRef<Obstacle list>) =

            // builds a sg from an obstacle description
            let toSg (o : Obstacle) =
                let p = Path.toWorldSpace o.position o.path

                o.sg
                    |> Sg.trafo o.rotation
                    |> Sg.trafo (p |> Mod.map Trafo3d.Translation)

            // build sg set
            instances 
                |> ASet.ofMod
                |> ASet.collect'' (fun v -> v |> List.map toSg :> seq<_>)