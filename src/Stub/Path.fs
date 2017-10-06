namespace SamyTheSalmon

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application

module Path =

    open System.IO
    open Aardvark.Base.Ag
    open Aardvark.SceneGraph.Semantics

    // semantics for Points() attribute
    [<Semantic>]
    type PathSemantics() =

        member x.Points(v : seq<V3d>) : alist<V3d> =
            AList.ofSeq v

        member x.Points(a : Sg.AdapterNode) : alist<V3d> =
            a.Node?Points() 

        member x.Points(t : Sg.TrafoApplicator) : alist<V3d> =
            let verts = x.Points (t :> IApplicator)

            alist {
                let! trafo = t.Trafo
                yield! verts |> AList.map (fun x -> trafo.Forward.TransformPos x)
            }

        member x.Points(n : IApplicator) : alist<V3d> = 
            alist {
                let! c = n.Child
                yield! c?Points()
            }


    // load vertices from an obj-file
    // assimp does not really do what we want, so we have to read it manually
    let load (file : string) =

        let lines =
            seq {
                use f = new StreamReader(file)
                while not f.EndOfStream do
                    yield f.ReadLine ()
            }

        lines
            |> Seq.filter (fun s -> s.StartsWith "v")
            |> Seq.map (fun s -> (s.Substring 2).SplitOnWhitespace()
                                    |> Array.map (fun v -> Double.Parse(v, System.Globalization.CultureInfo.InvariantCulture))  
                                    |> V3d)
            |> Sg.AdapterNode

    // extract the points (world space) from the path using the synthesized attribute
    let points (sg : ISg) =
        let (pts : alist<V3d>) = sg?Points()
        pts |> AList.toArray

    // computes the points as positions on the path (i.e. as scalar 0 - 1)
    let positions (path : V3d []) =

        let dist =
            path
                |> Array.pairwise
                |> Array.map V3d.Distance
                |> Array.scan (+) 0.0

        dist |> Array.map (fun x -> x / (Array.last dist))

    // computes the world space coordinates of the given position on the path
    let toWorldSpace (t : IMod<double>) (path : V3d []) =
            
        // linear positions of the path points
        let ps = path |> positions

        adaptive {
            let! t = t

            // find the corresponding segment and the position within it
            let i = ps |> Array.tryFindIndex (fun x -> x > t) |> Option.defaultValue 1
            let t = (t - ps.[i - 1]) / (ps.[i] - ps.[i - 1])

            // interpolate between the two points of the segment
            return (1.0 - t) * path.[i - 1] +  t * path.[i]
        }

    // finds the closest position on the path, given some world space coordinates
    let toPosition (p : V3d) (path : V3d []) =

        // finds the index of the minimum element
        let minIndex (xs : float []) =
            let min = xs |> Array.min
            xs |> Array.tryFindIndex ((=) min)
               |> Option.defaultValue 0

        // finds the parameter t of the closest point from the given point to the given line
        let closestPoint (p : V3d) (p0 : V3d, p1 : V3d) =
            let mutable t = 0.0
            p.GetClosestPointOnLine (p0, p1, &t) |> ignore
            t

        // find the index of the closest segment
        let segment =
            path
                |> Array.pairwise
                |> Array.map Line3d
                |> Array.map p.GetMinimalDistanceTo
                |> minIndex

        // find position within that segment
        let t =
            path
                |> Array.pairwise
                |> Array.item segment
                |> closestPoint p

        // compute global position
        path
            |> positions
            |> Array.pairwise
            |> Array.item segment
            |> fun (t0, t1) -> t0 + t * (t1 - t0)            