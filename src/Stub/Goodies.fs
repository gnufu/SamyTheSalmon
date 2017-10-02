namespace SamyTheSalmon

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
open Aardvark.Base.Ag

module Goodies =

    let private rand = Random()

    // computes bounding box of a goodie
    let private bb (g : Goodie) =
        let goodieSg = g.sg |> Sg.trafo (g.position |> Mod.map Trafo3d.Translation)
        goodieSg?LocalBoundingBox() : IMod<Box3d> // TODO: ask if it is possible to cast to sphere3d

    // loads & creates goodies
    let private create (dt : double)  =
        
        let types = GameContent.Content.goodieType
        let pts = GameContent.Content.goodies
        let mutable tmp = list.Empty

        // create new goodie
        for p in pts do
            let spin = Array.init 3 (fun _ -> (rand.NextDouble() * 2.0 - 1.0) * (Math.PI / 2.0)) |> V3d
            let gd = {
                    sg = types.[types.Length |> rand.Next]
                    position = Mod.init p
                    rotation = Trafo3d.Rotation(spin) |> Mod.init
                    spin = spin
                    boundingbox = Unchecked.defaultof<_>
                }

            // append and add proper bounding box
            tmp <- { gd with boundingbox = bb gd } :: tmp
        tmp

    // creates & updates goodies
    let mutable initialized = false // only creating goodies once & not again if all are collected. maybe there is a better way to do this!
    let update (dt : double) (s : State) =
        
        let gds = s.goodies
        match s.goodies.instances.Value |> List.isEmpty  && not initialized with
        | true ->
            initialized <- true

            // create goodies
            gds.instances <== create dt

            // return new state
            { s with goodies = { instances = gds.instances } }
        | false -> 
            let inst =
                gds.instances
                |> Mod.force
                |> List.iter (fun g ->
                    g.rotation <=* Trafo3d.Rotation(g.spin * dt)
                    )
            
            // return new state
            //gds.instances <== inst
            s

[<AutoOpen>]
module SgGoodiesExtensions =

    module Sg =

        // builds a set of sgs from a list of goodies
        let goodies (instances : IModRef<Goodie list>) =

            // builds a sg from an goodie description
            let toSg (g : Goodie) =
                g.sg 
                |> Sg.trafo g.rotation
                |> Sg.trafo (g.position |> Mod.map Trafo3d.Translation )

            // build sg set
            instances 
                |> ASet.ofModSingle
                |> ASet.collect' (fun v -> v |> List.map toSg :> seq<_>)
