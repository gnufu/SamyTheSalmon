namespace SamyTheSalmon

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

module Fireflies = 

    let g = SomeHelpers.box C4b.Yellow (Box3f.FromCenterAndSize(V3f.OOO, V3f(0.001))) |> Sg.ofIndexedGeometry
    let fireflySg p = 
        g
        |> Sg.material (Mod.constant C4d.Yellow) (Mod.constant C4d.White) (Mod.constant C4d.DarkYellow) (Mod.constant 32.0)

    // updates firefly spot light positions
    let private updateLights (s : State) =
        let l = s.lights |> Mod.force
        let fireflies = s.fireflies.instances |> Mod.force
        for i in 1 .. l.Length - 1 do
            l.[i] <- fireflies.[i - 1].light |> Mod.force
        let nl = [| l.[0]; l.[1]; l.[2]; l.[3]; l.[4]; l.[5]; l.[6]; l.[7];|]
        s.lights <== nl

    // create fireflies
    let private create (dt : double) (s : State) =
        [| for p in GameContent.Content.fireflies do
            let pos = p |> Mod.init
            let ff =  
                {
                    sceneGraph = fireflySg pos
                    position = pos
                    t = dt |> Mod.init
                    light = Lighting.point' p 1.0 (C4d(0.0)) (C4d(0.6, 0.5, 0.0)) |> Mod.init 
                } 
            yield ff
        |] |> Array.toList

    let update (dt : double) (s : State) =
        
        let ff = s.fireflies
        match s.fireflies.instances.Value |> List.isEmpty with
        | true ->
            ff.instances <== create dt s
            let s = { s with fireflies = { instances = ff.instances } }
            updateLights s
            s
        | false -> 
            let inst = 
                ff.instances
                |> Mod.force
                |> List.map (fun f ->
                    let op = f.position |> Mod.force
                    let t = (f.t |> Mod.force) + dt
                    let np = V3d(((cos t) * 0.01) + op.X, ((sin t) * 0.01) + op.Y, op.Z)
                    f.t <== t
                    f.position <== np
                    f.light <== Lighting.point' np 0.5 (C4d(0.0)) (C4d(0.0, 1.0, 0.0))
                    f)
            ff.instances <== inst
            let s = { s with fireflies = { instances = ff.instances } }
            updateLights s
            s

[<AutoOpen>]
module SgFireflyExtionsions =

    module Sg =
        
        // builds a set of sgs from a list of fireflies
        let fireflies (instances : IModRef<Firefly list>) =

            // builds a sg from a firefly description
            let toSg (f : Firefly) = 
                f.sceneGraph
                |> Sg.trafo (f.position |> Mod.map Trafo3d.Translation)

            // build sg set
            instances 
                |> ASet.ofMod
                |> ASet.collect'' (fun v -> v |> List.map toSg :> seq<_>)
                