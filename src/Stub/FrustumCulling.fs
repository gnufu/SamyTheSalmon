namespace SamyTheSalmon

open System
open Aardvark.Base
open Aardvark.Base.Ag
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
open Aardvark.Application

[<AutoOpen>]
module FrustumCullingExtensions =

    module Sg =

        // box - frustum intersection test for GL style projection
        let private boxFrustumIntersection (box : Box3d) (t : M44d) =

            let inside (plane : V4d) =
                let mutable min = V3d.Zero
                let mutable max = V3d.Zero
                box.GetMinMaxInDirection (plane.XYZ, &min, &max)

                min.Dot(plane.XYZ) + plane.W >= 0.0 || max.Dot(plane.XYZ) + plane.W >= 0.0

            inside (t.R3 + t.R0)            // left
                && inside (t.R3 - t.R0)     // right
                && inside (t.R3 + t.R1)     // top
                && inside (t.R3 - t.R1)     // bottom
                && inside (t.R3 + t.R2)     // near
                && inside (t.R3 - t.R2)     // far

        // returns if the given sg is within the view frustum
        let cull (frustum : IMod<Trafo3d>) (sg : ISg) =
            adaptive {
                let! cullingEnabled = Config.Application.frustumCulling

                if not cullingEnabled then
                    return true
                else
                    let! bb = sg.LocalBoundingBox ()
                    let! viewProj = frustum

                    return boxFrustumIntersection bb viewProj.Forward
            }

        // culls the given set of sgs for the given view frustum
        let cullSet (frustum : IMod<Trafo3d>) (set : aset<ISg>) =
            set //|> ASet.filterM (cull frustum)
                |> Sg.set
